newPackage(
	"RandomCurvesOverVerySmallFiniteFields",
	Version => "0.3",
	Date => "March 20, 2018",
	Authors => {{Name => "Christian Bopp",
			   Email =>"bopp@math.uni-sb.de",
			   HomePage =>"http://www.math.uni-sb.de/ag-schreyer/index.php/people/researchers/75-christian-bopp"},
		    {Name => "Frank-Olaf Schreyer",
			   Email =>"schreyer@math.uni-sb.de",
			   HomePage =>"https://www.math.uni-sb.de/ag/schreyer"}},
	Headline=> "general canonical curves of genus <= 15 over fields with small characteristic",
	Keywords => {"Examples and Random Objects"},
	PackageImports => {"Elimination","Truncations"}
	)
    
export{"isSmoothCurve",
    "smoothCanonicalCurve",
    "canonicalCurveViaPlaneModel",
    "smoothCanonicalCurveViaPlaneModel",
    "hilbertNumerator",
    "expectedBetti",
    "randomHartshorneRaoModule",
    "smoothCanonicalCurveViaSpaceModel",
    "randomCanonicalCurveGenus8with8Points",
    "randomCurveGenus8Degree14inP6",
    "randomCurveGenus14Degree18inP6",
    "smoothCanonicalCurveGenus14",
    "hasFactor",
    "selectFactor",
    "getAuxilaryCurveAndPts",
    "getModuleN",
    "curveFromModule",
    "smoothCanonicalCurveGenus15",
    "Printing",
    "Details"}    


-- 14.09.2015
-- the aim is to provide functions which compute canonical curves over fields with very small characteristic
-- at the moment we are only interested in the odd genus cases
-- for g<=10 we use a construction via plane models
-- for g=11,13 we use a construction via space model
-- for g=14 the construction follows Verra's unirationality proof of M_14
-- for g=15 we want to use similar methods as in the "MatFac15" package by F.-O. Schreyer

--Bug-report
-- running the code for g=14 and p=2 multiple times, it can happen that an M2 error occurs
-- because M2 can no longer compute the genus of some ideal (even the irrelevant ideal) 
-- for genus 15 this might as well happen, but we catch this cases by using the "try" command

-- NOTE:
-- The option "Details" for the genus 14 and 15 construction is only for testing / tweaking the construction:
-- it prints the precise step of the computation /its failure

--===========================================================================================================================--
--===========================================================================================================================--
--===============================	      SOME HANDY FUNCTIONS  	    	=============================================--
--===========================================================================================================================--
--===========================================================================================================================--

-- this function is a refinement of the function "isSmoothCurve" from Matfac15-package
isSmoothCurve = method(TypicalValue => Boolean)
isSmoothCurve (Ideal) := C -> (
    S := ring C;
    kk := coefficientRing S;
    if not (dim C == 2) then error "isSmoothCurve: expected the ideal of a curve";
    -- if embedded points then 
    n := dim S;    
    if n <= 4 then (
	-- check for no associated points
        if not dim Ext^(codim C)(C,S) <= 0 then (use S;<<"isSmoothCurve: there are associated points" <<endl; return false);
	-- check smoothness		
	singC := C +minors (n-2, jacobian C);	
	return(dim singC <= 0));
    if n>4 then (
	--projectionCenter:=ideal(apply(4,i->S_i));-- we should use a random projection center here and iterate a few times!!!!
	while(-- deg C == degC1
	while(-- dim projectionCenter == 0 and dim(projectionCenter+C) == 0
	projectionCenter := ideal(apply(4,i->random(1,S)));
	dim(projectionCenter+C) != 0 or dim(projectionCenter+C) != 0) do();
        x := getSymbol"x";
	y := getSymbol"y";	  
	PP := kk[x_0..x_3,y_0..y_(n-5)];
	f := map(S,PP, gens(projectionCenter) | random(S^1,S^{n-4:-1}) ); 
	C' := preimage_f(C);
	--C1'=eliminate(C' + ideal(x_0..x_3), toList(x_0..x_3));
        --if not dim(projectionCenter+C) == 0 then (use S; <<"bad projection center" <<endl; return false);
--	use S; 
	C1 := eliminate(C',toList(apply(n-4,i->PP_(i+4))));
	 not (degree C1 == degree C)) do();
	S1 := kk[x_0..x_3]; 
	C2 := substitute(C1,S1);
        if not dim Ext^2(C2,S1) <= 0 then (use S;<<"isSmoothCurve: there are associated points after projection" <<endl; return false);
	-- check smoothness		
	singC2 := C2 +minors (2,jacobian C2);use S;
	return(dim singC2 <= 0)
	);
    ) 

----------------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------------

--This function puts everything together
smoothCanonicalCurve = method(Options => {Details => false, Printing => false})
smoothCanonicalCurve (ZZ,ZZ) := opt -> (g,p) -> (
    if p == 57 then error "57 is the Grotehdieck prime number";
    if isPrime(p) == false then error "p is not prime";
    if (g > 15) then error"not implemented yet";
    if (g < 11) then (
                   return smoothCanonicalCurveViaPlaneModel(g,p, Printing => opt.Printing)
		   );
    if (g < 14 and g > 10) then (
                   return smoothCanonicalCurveViaSpaceModel(g,p, Printing => opt.Printing)
		   );	               	
    if (g == 14) then (
                   return smoothCanonicalCurveGenus14(p, Details => opt.Details, Printing => opt.Printing)		   );
    if (g == 15) then (
                   return smoothCanonicalCurveGenus15(p, Details => opt.Details, Printing => opt.Printing)
		   );
    return null);


--===========================================================================================================================--
--===========================================================================================================================--
--===============================	      CURVES VIA PLANE MODEL  	    	=============================================--
--===========================================================================================================================--
--===========================================================================================================================--


--Input: genus g, char p
-- output: ideal of canonically embedded curve of genus g over F_p
canonicalCurveViaPlaneModel = method(Options => {Printing => false})
canonicalCurveViaPlaneModel (ZZ,ZZ) := opt -> (g,p) -> (
  if isPrime(p) == false then error "p is not prime";
  while( -- get curve data correct
-- we determine the generic degree d of the plane model and the number of double points
  s := floor(g/3);
  d := g+2-s;
  delta := binomial(d-1,2)-g;
-- now we construct delta points in P^2 given by an Hilbert-Burch-matrix
  kk := ZZ/p;
  u := getSymbol "u";
  P2 := kk[u_0,u_1,u_2];
--the construction of the correct Hilbert-Burch matrix is based on the M2-package randomPlaneCurves.m2
  n := ceiling((-3+sqrt(9.0+8*delta))/2); 
  eps := delta-binomial(n+1,2);
   if opt.Printing then  (print("--> computing plane curve of degree "|toString(d)|" with "|toString(delta)|" double points"));  
  while(
     betti(M := random(P2^{n+1-eps:0,2*eps-n:-1},P2^{n-2*eps:-1,eps:-2}));
     points := minors(rank source M,M);
     singularPoints := points + minors(2,jacobian(points));
       not (degree points == delta and codim points == 2 and dim singularPoints == 0)) do ();
--Now we construct the plane model and a basis for the canonical system
  points2 := saturate(points^2);
  while(
    Cplane := ideal ((gens points2)*random(source gens points2, P2^{1:-d}));
    betti(K := gens intersect(points, (ideal vars P2)^(d-3)));
      not (source K == P2^{g:-(d-3)})) do ();
-- next we embed the curve in P^{g-1}  
  t := getSymbol "t";
  T := kk[t_0..t_(g-1)];
   if opt.Printing then  (print("--> computing canonical embedding"));  
  canEmb := map(P2,T,K);
  Ican := saturate ideal mingens preimage_canEmb(Cplane);
--test:
  ((dim Ican, degree Ican, genus Ican) != (2,2*g-2,g) or numgens Ican != 1/2*(g-2)*(g-3) or unique flatten degrees Ican != {2})) do ();
  --if  (dim Ican, degree Ican, genus Ican)!=(2,2*g-2,g) then error "something went wrong (genus, dim or degree)";
  --if numgens Ican != 1/2*(g-2)*(g-3) then error "something went wrong (number of generators)";
  --if unique flatten degrees Ican! = {2} then error "something went wrong (degree of generators)";
  Ican );

undocumented { canonicalCurveViaPlaneModel, (canonicalCurveViaPlaneModel,ZZ,ZZ), [canonicalCurveViaPlaneModel,Printing] } 
----------------------------------------------------------------------------------------------------------------------------

smoothCanonicalCurveViaPlaneModel = method(Options => {Printing => false})
smoothCanonicalCurveViaPlaneModel (ZZ,ZZ) := opt -> (g,p) -> (
smoothLimit :=20 ;
    while (
   Ican := canonicalCurveViaPlaneModel(g,p,Printing=>opt.Printing);
    counterSmoothness := 0;
    if opt.Printing then  (print("--> testing smoothness"));
    while(
    counterSmoothness = counterSmoothness+1;
    not( isSmoothCurve(Ican) or counterSmoothness >= smoothLimit )) do();
    counterSmoothness == smoothLimit ) do();
    Ican);

--===========================================================================================================================--
--===========================================================================================================================--
--===============================	      CURVES VIA SPACE MODELS  	    	=============================================--
--===========================================================================================================================--
--===========================================================================================================================--

--First we define some functions from the "RandomSpaceCurves"-package

------------------------------------
-- Hilbert Function and Numerator --
------------------------------------

-- calculate the numerator of a Hilbert function 
-- from the first d+r+1 values where
-- d is the regularity of the corresponding module
-- and r is the dimension of the ambient space
--
-- L = a list of dimensions
-- r = the dimension of the ambient space
-- t = the variable to be used in the numerator
hilbertNumerator = method()
hilbertNumerator (List,ZZ,RingElement) := (L,r,t) -> (
     -- the beginning of the hilbert series
     p := sum(#L,i -> L#i*t^i); 
     -- the numerator
     p*(1-t)^(r+1)%t^(#L)
     );

undocumented { hilbertNumerator, (hilbertNumerator,List,ZZ,RingElement) } 
----------------------------------------------------------------------------------------------------------------------------   

-----------------------------
-- Expected Betti Tableaus --
-----------------------------

-- convert c*t^d to (c,({d},d))
-- assumes only one term c*t^d
-- ring of t must be over ZZ or QQ
-- and singly graded
--
-- this function is needed to construct 
-- expected betti tables from
-- a HilberNumerator
termToBettiKey = (mon) -> (
     -- the coefficient of the monomial
     c := lift((last coefficients mon)_0_0,ZZ);
     -- the degree of the monmial
     d := sum degree mon;
     (c,({d},d))
     );
 
----------------------------------------------------------------------------------------------------------------------------


-- construct a minimal free resolution with expected betti tableau
expectedBetti = method()


-- calculates the expected betti tableau
-- from a hilbert Numerator
--
-- For this every term a_i*t^i will represent a summand R^{abs(a_i):-i}
-- in the ChainComplex represented by the desired BettiTableau
-- The step where this summand is used depends on the number of
-- sign switches that occur in the Hilbert numerator before this monomial  
--
-- the ring of the hilbert numerator is expected to singly graded 
-- and contain only one variable
expectedBetti (RingElement) := (hilbNum) -> (
     -- find terms of hilbert Numerator
     -- smallest degree first
     termsHilbNum := reverse terms hilbNum;
     -- convert terms into pairs (coefficient, ({d},d))
     bettiKeys := apply(termsHilbNum,m->termToBettiKey(m));
     -- put the summands into the appropriate step of F
     -- j contains the current step
     j := -1;
     -- previous Coefficient is needed to detect sign changes
     previousCoefficient := -(first bettiKeys)#0;
     -- step through all keys and calculate which step a
     -- given entry must go based on the number of sign-changes
     L := for b in bettiKeys list (
	  -- has a sign change occurred?
     	  if (b#0*previousCoefficient) < 0 then (
	       -- sign change => next step in the resolution
	       j = j+1;
	       );
	  -- store previous coefficient
     	  previousCoefficient = b#0;
	  -- make entry for the betti Tally
	  (prepend(j,b#1) => abs(b#0))
     	  );
     -- return the complex
     new BettiTally from L
     );


-- calculate the expected betti tableau
-- from a given hilbert function.
-- hilb = {h0,...,h_(d+r+1)} 
-- where d is the regularity of the variety described 
-- and r is the dimension of the ambient space
expectedBetti (List,ZZ) := (L,r) -> (
     t := local t;
     T := QQ[t];
     expectedBetti(hilbertNumerator(L,r,t))
     );



-- calculate the expected betti tableau
-- for a curve of degree d, genus g in IP^r.
-- we assume C non-degenerate, O_C(2) nonspecial and maximal rank
expectedBetti (ZZ,ZZ,ZZ) := (g,r,d) -> (
     b := d+r+1;
     L := apply(b,i->(if i>1 then 
	       min(d*i+1-g,binomial(r+i,r)) 
	       else binomial(r+i,r)));
     expectedBetti(L,r)
     );

undocumented { expectedBetti, (expectedBetti,RingElement), (expectedBetti, List,ZZ), (expectedBetti, ZZ,ZZ,ZZ) } 
----------------------------------------------------------------------------------------------------------------------------

-- given a betti Table b and a Ring R make a chainComplex 
-- with zero maps over R  that has betti diagram b. 
--
-- negative entries are ignored
-- rational entries produce an error
-- multigraded R's work only if the betti Tally
-- contains degrees of the correct degree length
Ring ^ BettiTally := (R,b) -> (
     F := new ChainComplex;
     F.ring = R;
     --apply(pDim b,i->F_i = null);
     for k in keys b do (
	  -- the keys of a betti table have the form
	  -- (homological degree, multidegree, weight)
	  (i,d,h) := k;
	  -- use F_i since it gives 0 if F#0 is not defined
	  F#i = F_i ++ R^{b#k:-d};
	  );
     F
     );

----------------------------------------------------------------------------------------------------------------------------

--------------------
-- Finite Modules --
--------------------

-- calculate the number of expected syzygies of a
-- random a x b matrix with linear entries in R
expectedLinearSyzygies = (a,b,R) -> (
     n := dim R;
     b*n-a*binomial(n+1,2)
     );

----------------------------------------------------------------------------------------------------------------------------


-- Try to construct a random HartshorneRao module of
-- length 3 starting at the beginning of the
-- minimal free resolution.
-- 
-- The main difficulty is in getting the number of 
-- linear syzygies of the first matrix in the resolution right
--
-- HRau = {h1,h2,h3} the Hilbertfunction of the desired module 
-- R the ring where the module should live. It is assumed, that 
-- this ring has 4 variables and is singly graded.
randomHartshorneRaoModuleDiameter3oneDirection = (HRao,R) -> (
     -- construct a chain complex with expected betti tableau
     -- and 0 differentials
     -- 
     -- calculate the expected betti diagram to find out whether linear syzygies 
     -- are required (this is the difficult part in the construction)
     e := expectedBetti(HRao|{0,0,0,0},3);
     F := R^e;
     -- find betti Numbers of the linear strand
     linearStrand := for i from 0 list (if e#?(i,{i},i) then e#(i,{i},i) else break);
     -- construction depends on length of linear strand.
     if #linearStrand == 0 then error"linear Stand has length 0. This should never happen";
     if #linearStrand == 1 then (
	  -- first matrix can neither have nor be required to have linear syzygies
	  -- choose first matrix randomly
     	  return coker random (F_0,F_1)
	  );
     if #linearStrand == 2 then (
	  -- no linear syzygies of the first matrix are required
	  -- check if first matrix always has unwanted syzygies
	  if expectedLinearSyzygies(linearStrand#0,linearStrand#1,R) <= 0 then (
	       -- no unwanted syzygies
	       -- choose first matrix randomly
     	       return coker random (F_0,F_1)
	       );
     	  );	       
     if #linearStrand == 3 then (
	  -- is the number of expected syzygies == the number of required syzygies?
	  if expectedLinearSyzygies(linearStrand#0,linearStrand#1,R) == linearStrand#2 then (
	       -- choose first matrix randomly
     	       return coker random (F_0,F_1)
	       );
	  -- too many syzygies?
	  if expectedLinearSyzygies(linearStrand#0,linearStrand#1,R) > linearStrand#2 then (
	       -- in this case the construction method will not work
	       return null     	       
	       );
	  -- too few syzygies?
	  if expectedLinearSyzygies(linearStrand#0,linearStrand#1,R) < linearStrand#2 then (
	       -- try to choose the syzygies first
	       -- this will work if the transpose of a generic map between
	       -- 1. and 2. module of the linear strand has more expected syzygies
	       -- than required in the 0. step
     	       if expectedLinearSyzygies(linearStrand#2,linearStrand#1,R) >= linearStrand#0 then (
	       	    -- syzygies of the transpose of second step in linear strand
	       	    s := syz random(R^{linearStrand#2:2},R^{linearStrand#1:1});
	       	    -- choose linearStrand#0 syzygies randomly among those and transpose again
	       	    return coker (transpose (s*random(source s,R^{linearStrand#0:0})));
	       	    );
	       )
      	   );
      -- if we arrive here there were either to few or to many linear
      -- syzygies required	  
      return null     	       
      );

----------------------------------------------------------------------------------------------------------------------------

-- Try to construct a random Hartshorne-Rau module of
-- length 3 by starting at both ends of the expected
-- minimal free resolution.
--
-- HRau = {h1,h2,h3} the Hilbertfunction of the desired module 
-- R the ring where the module should live. It is assumed, that 
-- this ring singly graded. It is checked that the ring has 4 variables
randomHartshorneRaoModuleDiameter3 = (HRao,R) -> (
     if #HRao != 3 then error"Hilbert function has to have length 3";
     -- start at the beginning of the resolution    
     M := randomHartshorneRaoModuleDiameter3oneDirection(HRao,R);
     -- did this direction work?
     if M =!= null and apply(3,i->hilbertFunction(i,M)) == HRao then return M;
     -- start at the end of the resolution
     Mdual := randomHartshorneRaoModuleDiameter3oneDirection(reverse HRao,R);
     --Mdual==null-- comment out
     if Mdual===null then return M;
     Fdual := res Mdual;
     M = (coker transpose Fdual.dd_4)**R^{ -6};
     return M
     );
 

----------------------------------------------------------------------------------------------------------------------------

-- for g=11,12,13 we will only need the diameter 3 part, but we also include the functions for diameter 1 and 2:
-- Try to construct a random Hartshorne-Rau module of
-- length 2. Here the only problem is, that the
-- generic module may not have expected syzgies
--
-- HRau = {h1,h2} the Hilbertfunction of the desired module 
-- R the ring where the module should live. It is assumed, that 
-- this ring has 4 variables and is singly graded.
randomHartshorneRaoModuleDiameter2 = (HRao,R) -> (
     if #HRao != 2 then error"Hilbert function has to have length 2";
     -- some special cases with non expected resolution
     --
     --if HRao == {1,1} then return coker random(R^{0},R^{3:-1,1:-2});
     --if HRao == {1,2} then return coker random(R^{0},R^{2:-1,3:-2});
     --if HRao == {2,1} then return coker random(R^{2:0},R^{7:-1});
     --
     -- the standard construction still works since the unexpected
     -- part is not in the first 2 steps.
     --
     -- now assume expected resolution
     --
     -- always start at the beginning of the resolution  
     F := R^(expectedBetti(HRao|{0,0,0,0},3));
     M := coker random(F_0,F_1)
     );

----------------------------------------------------------------------------------------------------------------------------

-- Construct a random Hartshorne-Rau module of
-- length 1. This always works
--
-- HRau = {h1} the Hilbertfunction of the desired module 
-- R the ring where the module should live. It is assumed, that 
-- this ring has 4 variables and is singly graded.
randomHartshorneRaoModuleDiameter1 = (HRao,R) -> (
     if #HRao != 1 then error"Hilbert function has to have length 1";
     return coker (vars R**R^{HRao#0:0})
     );
 
----------------------------------------------------------------------------------------------------------------------------  

randomHartshorneRaoModule = method()
randomHartshorneRaoModule (ZZ,List,PolynomialRing) := (e,HRao,R) -> (
     if dim R != 4 then error "expected a polynomial ring in 4 variables";
     if degrees R !={{1}, {1}, {1}, {1}} then error "polynomial ring is not standard graded";
     if #HRao > 3 then error "no method implemented for Hartshorne Rao module of diameter >3";
     M := null;
     while(
     if #HRao == 1 then M = randomHartshorneRaoModuleDiameter1(HRao,R);
     if #HRao == 2 then M = randomHartshorneRaoModuleDiameter2(HRao,R);
     if #HRao == 3 then M = randomHartshorneRaoModuleDiameter3(HRao,R);
     ( M === null )) do(); 
     M**R^{ -e}
     );

undocumented { randomHartshorneRaoModule, (randomHartshorneRaoModule,ZZ,List,PolynomialRing) } 
----------------------------------------------------------------------------------------------------------------------------

--Input: genus g, char p
-- output: ideal of canonically embedded curve of genus g over F_p
smoothCanonicalCurveViaSpaceModel = method(Options => {Printing => false})
smoothCanonicalCurveViaSpaceModel (ZZ,ZZ) := opt -> (g,p) -> (
  kk := ZZ/p;
  if isPrime(p) == false then error "p is not prime";
-- costruction of space model of degree d and genus g
-- therefore we first construct the HR-module
  d := g+4-floor(g/3);
  if opt.Printing then  (print("--> computing space curve of genus "|toString(g)|" and degree "|toString(d)));
  y := getSymbol "y";
  R := kk[y_0..y_3]; 
-- calculate values of h^1 that are forced by the maximal rank assumption
  h1 := for i from 0 when ((i < 4) or(d*i+1-g) > binomial(i+3,3)) list max(d*i+1-g-binomial(3+i,3),0);
  e := 0; for i in h1 when i == 0 do e = e+1;
-- calculate support of Hartshorne Rao Moduole
  HRao := select(h1,i->i!=0);  
  expBettiHR := expectedBetti(HRao|{0,0,0,0},3);  
  emptyResHR := R^expBettiHR;
-- depending if the genus is 11,12 or 13 the length of the linear strand of the expected resolution of the HR-modules differs
-- therefore we distinguish 2 cases:
-- a) length linear strand==2 (g=12):
-- b) length linear strand==3 (g=11,13):
   linearStrand := for i from 0 list (if expBettiHR#?(i,{i},i) then expBettiHR#(i,{i},i) else break);
--  while ( --loops don't work for g=12
--  if (g==12) then (Mpres=  random (emptyResHR_0,emptyResHR_1)) --the presentation of the HR-module if #lengthLinStrand==2
--  else   (s:= syz random(R^{linearStrand#2:2},R^{linearStrand#1:1});
--          Mpres= (transpose (s*random(source s,R^{linearStrand#0:0}))); --the presentation of the HR-module if #lengthLinStrand==3
--	  );
--   M:=(coker Mpres)**R^{ -e}; -- the HR-module
--   not (select(subsets(rank source Mpres,rank target Mpres),l->det(Mpres_l)==0)=={} and betti(resHR=res coker Mpres)  == expBettiHR )--test --apply(2..5,i->hilbertFunction(i,M)) == HRao
--         ) do() --need maybe dual construction
     --another constr using the package
if opt.Printing then  (print("--> testing smoothness"));     
  while(--get smooth curve 
  while(--get dimension correct      
  while( M := (randomHartshorneRaoModule)(e,HRao,R); 
             Mpres := presentation M;
      not (select(subsets(rank source Mpres,rank target Mpres),l->det(Mpres_l) == 0) == {} 
	     and betti(resHR := res coker Mpres**R^{e})  == expBettiHR  
	     and    toList(apply(2..4,i->hilbertFunction(i,M))) == HRao)--tests
	) do();
-- we can now construct the space model of the curve from the expected resolution of the coordinate ring of C and the HR-module
-- this part is baes on the function "randomSpaceCurve" in the package "randomSpaceCurves.m2"
  expBettiC := expectedBetti(g,dim R-1,d); 
  emptyResC := R^expBettiC;
-- detect syzygies in the second step, that do not 
-- come from the HR-Module
  resHR = res M;
  H := R^((betti emptyResC_2)-(betti resHR_3));
  --while(  --needs to long
  N := random(emptyResC_1,resHR_2++H_0)*(resHR.dd_3++id_(H_0));
--  not (select(subsets(rank source N,rank target N),l->det(N_l)==0)=={})
       --) do();
  I := ideal syz transpose N; 
  not(dim I == 2)) do();  --maybe it is enough to rerun just a part of this function
  not (isSmoothCurve(I))) do() ; 
--(dim I, genus I, degree I, betti res I)==(2,g,d, expBettiC)
-- now we embed the space curve canonically
  t := getSymbol"t";
  T := kk[t_0..t_(g-1)];
if opt.Printing then  (print("--> computing canonical embedding"));  
  S := T**R;
  omegaC := presentation prune truncate(0,Ext^1(I,R^{ -4}));
  graph := substitute(vars T,S)*substitute(omegaC,S);
  while (-- get canonical embedding correct
  J := saturate(ideal graph, substitute(random(1,R),S));--why not random(1,R) instead of y_0?
  Ican := ideal mingens substitute(J,T);
  not((dim Ican, genus Ican, degree Ican) == (2,g,2*g-2))) do();
  Ican
) 


--===========================================================================================================================--
--===========================================================================================================================--
--===============================	      CURVES OF GENUS 14 	    	=============================================--
--===========================================================================================================================--
--===========================================================================================================================--


--Input: the characteristic 
--Output: a pair of an ideal of a canonical curve C
--        together with a list of ideals of 8 points
--Method: Mukai's structure theorem on genus 8 curves.
--  Note that the curves  have general Clifford index.
-- This function is similar to the function "randomCanonicalCurveGenus8with8Points" 
--  from the Macaulay2-Package "RandomGenus14Curves.m2"
--  For more information about this function see the Macaulay2-Package "RandomGenus14Curves.m2"
randomCanonicalCurveGenus8with8Points = method()
randomCanonicalCurveGenus8with8Points (ZZ) := p -> (
    kk := ZZ/p;
     x := symbol x;
     R := kk[x_0..x_7];
     q := symbol q;
     -- coordinate ring of the Plücker space:
     P := kk[flatten apply(6,j->apply(j,i->q_(i,j)))];
     skewMatrix := matrix table(6,6,
	  (i,j) -> (
	       if i<j then q_(i,j)
	       else if i>j then -q_(j,i)
	       else 0_P));
     -- ideal of the Grassmannian G(2,6):
     IGrass := pfaffians(4,skewMatrix);
     while( -- get data of the curve correct
     while (-- get points and dimension of their span correct
     while (
     points := apply(8,k->exteriorPower(2,random(P^2,P^6)));
     -- the 8 ideals corresponding to the 8 points
     ideals := apply(points,pt->ideal( vars P*(syz pt**P^{-1})));  
     -- linear span of the points:
     L1 := intersect ideals;
     not( degree L1 == 8 ) ) do();
     L := super basis(1,L1);
     not (dim ideal L == 8) ) do();
     phi := vars P%L; -- coordinates as function on the span
     -- actually the last 8 coordinates represent a basis
     phi2 := matrix{toList(7:0_R)}|vars R; 
     -- matrix for map from R to P/IC
     IC := ideal (gens IGrass%L); --the ideal of C on the span
     -- obtained as the reduction of the Grassmann equation mod L
     IC2 := ideal mingens substitute(IC,phi2);
     idealsOfPts := apply(ideals,Ipt->
         ideal mingens ideal sub(gens Ipt%L,phi2));
     not (dim IC2 == 2 and genus IC2 == 8 and #idealsOfPts == 8)) do();
     (IC2,idealsOfPts))
 
undocumented { randomCanonicalCurveGenus8with8Points, (randomCanonicalCurveGenus8with8Points,ZZ) }  

----------------------------------------------------------------------------------------------------------------------------

-- Input:  the characteristic
-- Output: ideal of a genus 8 degree 14 curve in P^6
-- This function is similar to the function "randomCurveGenus8Degree14inP6" 
--  from the Macaulay2-Package "RandomGenus14Curves.m2"
--  For more information about this function see the Macaulay2-Package "RandomGenus14Curves.m2"

-- this step seems to take some time over ZZ/2. TODO: Further tests for characteristic 2!!
randomCurveGenus8Degree14inP6 = method(Options => {Details => false})
randomCurveGenus8Degree14inP6 (ZZ) := opt -> p -> (
     y := getSymbol "y";
     kk := ZZ/p;
     S := kk[y_0..y_6];
  --if opt.Details then print"--> computing canonical genus 8 curve with 8 pts";     
     while( -- get correct data for IC3
     while ( --get Linearseries L correct
     while ( -- get data for D1 and D2 correct 	 	 
     (I,points) := randomCanonicalCurveGenus8with8Points(p);
     if opt.Details then print"--<< canonical genus 8 curve with 8 pts computed (check)";      
     R := ring I;
     if I === null then return null;
     D1 := intersect apply(4,i->points_i); -- divisors of degree 4 
     D2 := intersect apply(4,i->points_(4+i));
     if opt.Details then print"--<< testing D1 and D2";      
     not (degree D1 == 4 and degree D2 == 4)) do();
     -- compute the complete linear system |K+D1-D2|, note K=H1, the hyperplane section
     counter := 0;
     attempts := 200;
     --print"==>testing source L";    
     while( -- get correct dimension of the linear series
     counter = counter+1;
     H1 := gens D1*random(source gens D1,R^{-1});
     E1 := (I+ideal H1):D1; -- the residual divisor
     L := mingens ideal(gens intersect(E1,D2)%I);       
     not (source L == R^{7:-2}or counter >= attempts )) do();
     --print"==>testing counter in source L loop";  
     counter == attempts ) do();
     -- the complete linear system
     -- note: all generatore of the intersection have degree 2.
     RI := R/I; -- coordinate ring of C' in P^7
     phi := map(RI,S,substitute(L,RI));
     IC3 := ideal mingens ker phi;
     if opt.Details then print"--<< testing data of IC3";   
     -- dim seems to work but the genus and degree take strange values (-5,5)..(1,7) etc   
     not( dim IC3 == 2 and genus  IC3 == 8 and degree IC3 == 14)) do();
     IC3)

undocumented { randomCurveGenus8Degree14inP6, (randomCurveGenus8Degree14inP6,ZZ), [randomCurveGenus8Degree14inP6,Details]  } 

----------------------------------------------------------------------------------------------------------------------------

-- Input: S PolynomialRing in 7 variables
-- Output: ideal of a curve of genus 14
-- Method: Verra's proof of the unirationality of M_14

--  This function is similar to the function "randomCurveGenus14Degree18inP6" 
--  from the Macaulay2-Package "RandomGenus14Curves.m2"
--  For more information about this function see the Macaulay2-Package "RandomGenus14Curves.m2"
randomCurveGenus14Degree18inP6 = method(Options => {Details => false, Printing => false})
randomCurveGenus14Degree18inP6 (ZZ) := opt -> p -> (
     while(
     if opt.Details then print"--<< computing Genus 8 degree 14 curve in P^6";
     if (opt.Details or opt.Printing) then print("--> computing genus 8 degree 14 curve in P^6"); 
     IC' := randomCurveGenus8Degree14inP6(p, Details => opt.Details);
     if opt.Details then print"--<< (check) computing Genus 8 degree 14 curve in P^6";      
     S := ring IC';
     if IC' === null then return null;
     if (opt.Details or opt.Printing) then print"--> computing genus 14 degree 18 curve in P^6";
     -- Choose a complete intersection:
     counter := 0;
     attempts := 30;
     while(
     CI := ideal (gens IC'*random(source gens IC',S^{5:-2}));
     IC := CI:IC'; -- the desired residual curve
     counter = counter+1;
     not((dim IC == 2 and genus IC == 14 and degree IC == 18) or counter >= attempts) ) do();
     counter == attempts) do();
     IC)

undocumented { randomCurveGenus14Degree18inP6, (randomCurveGenus14Degree18inP6,ZZ), [randomCurveGenus14Degree18inP6,Printing],[randomCurveGenus14Degree18inP6,Details]} 

----------------------------------------------------------------------------------------------------------------------------

smoothCanonicalCurveGenus14 = method(Options => {Details => false, Printing => false})
smoothCanonicalCurveGenus14 (ZZ) := opt -> p -> (
	  smoothLimit := 20;
	  while(
     	       I := randomCurveGenus14Degree18inP6(p,Details => opt.Details, Printing => opt.Printing);
	       --if opt.Printing then print"-->checking smoothnessof C'" ;
	       counterSmoothness := 0;
	       if (opt.Details or opt.Printing) then print("--> testing smoothness");
	       while(-- test smoothness multiple times
     	       counterSmoothness = counterSmoothness+1;
     	       not( isSmoothCurve(I) or counterSmoothness >= smoothLimit )) do();
     	       counterSmoothness == smoothLimit ) do();
	  if (opt.Details or opt.Printing) then print"--> computing canonical embedding" ;
	  S := ring I;
	  kk := coefficientRing S;
	  --time omegaC := presentation  truncate(0, Ext^4(I,S^{ -7}));
	  -- the following seems faster:
	  fI := res I;
	  omegaC := presentation truncate(0,((coker transpose fI.dd_5)**S^{ -7}));
     	  t := getSymbol "t";
     	  T := kk[t_0..t_13];
     	  TS := T**S;
     	  graph := substitute(vars T,TS)*substitute(omegaC,TS);          
          while(
     	      J := saturate(ideal graph, substitute(random(1,S),TS));
     	      Ican := ideal mingens substitute(J,T);
	      not((dim Ican, genus Ican, degree Ican) == (2,14,26))) do();
	  Ican) 

--===========================================================================================================================--
--===========================================================================================================================--
--===============================	      CURVES OF GENUS 15 	    	=============================================--
--===========================================================================================================================--
--===========================================================================================================================--

--TODO: Write better comments to the functions

-- hasFactor  is contained in the Glicci package
hasFactor = method(TypicalValue=>Boolean)

hasFactor (RingElement,ZZ) := (f,n) -> (
     hasFactor(ideal f,n)
     )
     
hasFactor (Ideal,ZZ) := (I,n) -> (
     -- check whether a homogeneous principal ideal I in two variables over finite ground field FF
     -- is square-free and has a factor of degee n defined over FF. 
     R := ring I;
     if not class R === PolynomialRing and dim R !=2 then error "expected a polynomial in P^1";
     cp := decompose I; -- decompose I in its irreducible factors
     t := tally apply(cp,c -> degree c); -- frequency of degree c factors
     dc := unique select(apply(cp,c -> degree c),d -> d <= n); --degrees of factors of degree at most n
     if sum apply(cp,c -> degree c) =!= degree I then return false; -- check that f has no multiple factor
     L := {0};
     Ld := 0;
     --sum the degrees of subsets of factors of degree <=n recursively
     scan(dc,d -> (Ld = apply(t_d+1,i -> i*d); 	L = flatten apply(L,l -> apply(Ld,k -> l+k))));
     member(n,L)
     )

undocumented { hasFactor, (hasFactor,RingElement,ZZ), (hasFactor,Ideal,ZZ) }  
----------------------------------------------------------------------------------------------------------------------------

-- selectFactor is contained in the Glicci package
selectFactor = method()
selectFactor (Ideal,ZZ) := (J,n) -> (
     S := ring J;
     kk := coefficientRing S;
     --project to a line
     S1 := kk[gens S,MonomialOrder => Eliminate 2];
     S2 := kk[S_2,S_3];
     J1 := sub(J,S1);
     f := sub(selectInSubring(1,gens gb  J1),S2);
     cp := decompose ideal f;
     t := tally apply(cp,c -> degree c); -- frequencies in which a factor occurs
     dc := unique select(apply(cp,c->degree c),d -> d <= n);
     L := {{0,{{0,0}}}}; 
     Ld:=0;
     -- we build a list of factors consisting of tuples {m,L1}, of a possible degree m of a factor and a list L1 of tuples
     -- {d,i}, with d the degree of an irreducible factor and 0 <= i <= t_d the number of factors we take in this degree
     scan(dc,d -> (Ld = apply(t_d+1,i -> {i*d,{{d,i}}});
	         L = flatten apply(L,l -> apply(Ld,k -> {l_0+k_0,l_1|k_1})))
	    );
     A := (select(L,l -> l_0 == n))_0_1; -- select takes all possibilities to reach the desired degree n, 
     --_0 takes the first possibility
     -- and _1 the list L1 of {d,i} of degrees d and number i of factors of degree d 
     A1 := select(A,a -> a_1 > 0);-- remove factors with i=0, i.e. those we did not use
     fs := 0; 
      try (fn := product(A1,a -> (fs = select(cp,c -> degree c == a_0); 
	                product(a_1,j -> fs_j))
		   )) else return S; -- compute the corresponding product
	      
     try(I1 := J:sub(fn,S)) else (I1 = J); -- compute the residual scheme to the scheme defined by sub(fn,S), the lift of the factor back to P3.
     --note that we do not check that the lifted factor has the right cardinality. This must be checked by the calling routine.
   try (J2 := J:I1) else J2 = ideal S;
    --return J:I1 --return the ideal of the subscheme corresponding to the selected factor.
   J2
     ) 

undocumented { selectFactor, (selectFactor, Ideal,ZZ) }   
---------------------------------------------------------------------------------------------------------------------------- 
getAuxilaryCurveAndPts = method()
getAuxilaryCurveAndPts (ZZ) := (p) -> (
     kk := ZZ/p;
     y := getSymbol "y";
     R := kk[y_0..y_3];
     -- the Chang-Ran construction of a degree 12 and genus 11 curve in P^3:
     while(--find good E together with points
     while(--find good E
     HRao := coker random(R^3,R^{6:-1,2:-2});
     betti(fRao := res HRao);
     betti (syzE := fRao.dd_2*random(fRao_2,R^{6:-3}));
     try (E := ideal syz syzE) else E = ideal vars R;-- todo: test if the try fct here is still relevant
     -- following line seems only relevant in char 2 due to strange errors
     try ((dim E, degree E,genus E) == (2,12,11)) else return (ideal vars R,0_R);-- comment in!!!
     not ((dim E, degree E,genus E) == (2,12,11)) ) do();
     S1 := kk[gens R,MonomialOrder => Eliminate 2]; --prepare projection to P^1
     S2 := kk[R_2,R_3];
     Epts := 0; Epts1 := 0; f := 0; 
     counter1 := 0;
     while ( -- deg==6
     	  while ( -- has factor of deg 6
     	       Epts = E+random(2,R);
     	       Epts1 = sub(Epts,S1);
     	       f = sub(selectInSubring(1,gens gb  Epts1),S2); -- project to P^1
	       counter1 = counter1+1;
	       not(hasFactor(ideal f,6) or counter1 >= 100)) do();
     	      -- (not hasFactor(ideal f,6)) and (counter1<100)) do (); 
     	 if counter1 < 100 then  pts := selectFactor(Epts,6) else pts = ideal R;
	  (degree pts =!= 6) and (counter1 < 100) ) do ();
      -- counter <100 should be sufficient
      (dim pts, degree pts) =!= (1,6)) do();
  (E,pts)
  )

undocumented { getAuxilaryCurveAndPts, (getAuxilaryCurveAndPts,ZZ) } 

----------------------------------------------------------------------------------------------------------------------------

getModuleN = method(Options=>{Details => false}) 
getModuleN (Ideal,Ideal) := opt -> (E,pts) -> (
--(E,pts)=getAuxilaryCurveAndPts(3);  
     R := ring E;
     kk := coefficientRing R;
     x := getSymbol"x";
     S := kk[x_0..x_4];
     N := module ideal(0);
 -- dim E, degree E, genus E, dim pts, degree pts
     --D6:=intersect pts;--D6=pts
     D6 := pts; -- just to make the notation the same as in the Matfac15-package
     omegaE := Ext^1(E,R^{ -4});
     counterK := 0;
      while(--dim K
     betti (K1 := presentation omegaE|random(target presentation omegaE,R^1));
     K := annihilator coker K1; -- a canonical divisor on E
     counterK = counterK+1;
     not ((dim K, degree K) == (1,20) or counterK >= 10)) do();--this loop might not terminate
     if ((dim K, degree K) =!= (1,20) and counterK == 10)  then( if opt.Details then print"--<< data K"; return N);
     counterH := 0;
     while(--rank source H
     H5 := ideal(gens K*random(source gens K,R^{ -5}));
     try (H := mingens ideal (gens intersect(H5+E:K,D6)%E) ) else H = mingens (ideal 1_(ring E));
     counterH = counterH+1;
     not(rank source H == 5 or counterH >= 10)) do(); -- this loop might not terminate!
     if ( rank source H =!= 5 and counterH == 10)  then (if opt.Details then print"--<< data H"; return N);    
     -- re-embed E into P^4
     RE := R/E;
     phi := map(RE,S,sub(H,RE));
     IE := ker phi;
     if (isHomogeneous(IE) == false) then (if opt.Details then print"data IE"; return N);-- error which might occurs if char(kk) small / maybe running the few line above again will fix this error
     if (not((dim IE, degree IE, genus IE) == (2,14,11))) then (if opt.Details then print"--<< data IE"; return N); -- if genus (or data) is wrong start with new auxiliary curve
     betti (L1 := presentation omegaE|random(target presentation omegaE,R^{1}));
     D := saturate annihilator coker L1;
     RD := R/D;
     D8 := ideal mingens ker map(RD,S,sub(H,RD));
     if ((dim D8,degree D8) =!= (1,8)) then (if opt.Details then print"--<< data D8"; return N); ----if dim=2 then start with new curve E !! but why? (also if other data incorrect?)
     counterResidual:=0;
     while( 
     H5' := ideal(gens D8*random(source gens D8,S^{ -5}));
     residual := (H5'+IE):D8;
     counterResidual = counterResidual+1;
     not(degree residual == 5*14-8 or counterResidual >= 10)) do();
     if (degree residual =!= 5*14-8 and counterResidual == 10) then (if opt.Details then print"--<< data residual"; return  N);
     betti (A := saturate residual);
     betti(A1 := syz gens A);
     genericBettiA1 := new BettiTally from {(0, {3}, 3) => 3, (0, {4}, 4) => 9,  (0, {5}, 5) => 2, (0, {6}, 6) => 2, (1, {5}, 5) => 24, (1, {7}, 7) => 14};
     if  (betti A1 =!= genericBettiA1) then (if opt.Details then print"--<< data A1"; return  N);
     betti(N1 := (transpose(transpose A1_{24..37})_{12..15})**S^{5});
     betti(N2 := N1*syz(random(S^{ -1},target N1)*N1,DegreeLimit=>2));    
     N = coker transpose syz transpose syz N2;-- further tests for N might be needed
     N)--fct close 

undocumented { getModuleN, (getModuleN,Ideal,Ideal), [getModuleN, Details] } 
----------------------------------------------------------------------------------------------------------------------------

--combines the functions MatrixfactorizationFromModule and CurveFromMatrixFactorization
-- it might happen, that the fct getModuleN produces a module,
-- for which the following fct does not terminate
curveFromModule = method(Options=>{Details => false}) 
curveFromModule (Module) := opt -> (N) -> (
    -- we first build the matrix factorization from the module N
    --if (opt.Details or opt.Printing) then print("--> building curve from module")
     SN := ring N;
     gIE := gens annihilator N;  
     counter := 0;  
     genericBettiM0 := new BettiTally from {(0, {0}, 0) => 3, (0, {1}, 1) => 15, (1, {2}, 2) => 18};
     genericBettiM1 := new BettiTally from {(0, {2}, 2) => 18, (1, {3}, 3) => 3, (1, {4}, 4) => 15} ;
     while(-- repeat this step until we rank source sms==4
     while (-- get matrix factorization data correct  
      -- since running this fct with a timelimit does not work, we have the following loop: 
       -- force an error in case this does not terminate (e.g. for char 2) 
     counter = counter+1; 
     if (opt.Details and counter == 100) then print("infinite loop");
     if counter == 100 then error;
     X' := ideal(gIE * random(source gIE,SN^{ -3}));
     SNX := SN/X';
     -- if something is wrong with N the next step might take "forever". Hence: alarm
     if opt.Details then print("--<< computing fNX");
     fNX := res (N**SNX);
     if (opt.Details and length fNX < 2) then print("--> length fNX < 2");
     (M0,M1) := (fNX.dd_5**SNX^{6},fNX.dd_6**SNX^{6});
     not (betti M0 == genericBettiM0 and betti M1 == genericBettiM1 )) do();
     -- and now build the curve from the matrix factorization
     SX := ring M0;
     bSX := SX.baseRings;
     S := last bSX;
     X := ideal SX;
     m0 := M0; m1 := M1;
--   (betti m0, betti m1) <<endl ;     
     if #unique degrees target m1 == 2 then (m0 = m1; m1 = syz m0);
     degs := apply(degrees source m1, d -> d_0);  
     d0 := min degs;
--   (betti m0,betti m1) <<endl;
     if #select(degs,d -> d == d0) == 15 then (
	  (m0,m1) = (transpose m1,transpose m0);
          degs = apply(degrees source m1, d-> d_0);  
          d0 = min degs);          
--   (betti m0, betti m1) << endl;
     d0s := select(rank source m1,i -> degs_i == d0);
     sm1 := syz(transpose m1_d0s,DegreeLimit=>-d0+1);
     -- if we are unlucky the following step takes forever. Therefore we set the alarm
     if opt.Details then print("--<< computing sms");
      sms := syz transpose (sm1|transpose m0);
     if  (opt.Details and rank(sms) == 0) then print("--<< sms failure");
     not(rank source sms >= 4)) do();
     C:=ideal mingens ((sub(ideal((transpose syz transpose sms_{0..2})*sms_{3}),S)+X));
--  (dim C,degree C, genus C) << endl;
     return C)

undocumented { curveFromModule, (curveFromModule,Module), [curveFromModule, Details] } 
----------------------------------------------------------------------------------------------------------------------------

smoothCanonicalCurveGenus15 = method(Options=>{Details => false, Printing=>false}) 
smoothCanonicalCurveGenus15 (ZZ) := opt -> (p) -> (
    expectedBettiN := new BettiTally from { (0,{0},0) => 2, (0,{1},1) => 1, (1,{2},2) => 9};
    expectedBettiSyzN := new BettiTally from { (0, {2}, 2) => 9, (1, {4}, 4) => 14};
    smoothLimit := 20;-- maybe include this in the input 
    --time while (-- get smooth curve 
    --time while(-- get data of curve in P4 correct
    while (-- get smooth curve 
    while(-- get data of curve in P4 correct     
    while( -- data of N    
    while ( -- get good E (this loop seems only relevant for char 2, i.e. errors in the getAuxillaryCurve fct)    
    (E,pts) := getAuxilaryCurveAndPts(p);
    not(dim E == 2) ) do();
    if (opt.Details or opt.Printing) then print("computing the matrix factorization");
    N := getModuleN(E,pts, Details => opt.Details);-- sometimes it seems to be sufficient to reroll this fct
    bettiSyzN := betti syz presentation N;
    (N == 0 or (betti N =!= expectedBettiN) or (bettiSyzN =!= expectedBettiSyzN))) do();-- N might still ne chooses badly--> one step in the curveFromModule fct takes forever
    if (opt.Details or opt.Printing) then print"--> building curve from matrixfactorization";
    --IC:=curveFromModule(N); --might not terminate
    --try (alarm 60; IC:=curveFromModule(N)) else IC=ideal ring N; -- time limit useless
    try(IC := curveFromModule(N, Details=>opt.Details)) else IC = ideal ring N;
    ((dim IC, genus IC, degree IC) != (2,15,16))) do();
     counterSmoothness := 0;
     if (opt.Details or opt.Printing) then print"--> testing smoothness";
     while(-- test smoothness multiple times
     counterSmoothness = counterSmoothness+1;
     not( isSmoothCurve(IC) or counterSmoothness >= smoothLimit )) do();
     counterSmoothness == smoothLimit ) do();
     -- now the canonical embedding:
     if (opt.Details or opt.Printing) then print"--> computing canonical embedding";
     S := ring IC;
     kk := coefficientRing S;
     omegaC := presentation  truncate(0, Ext^2(IC,S^{ -5}));
     t := getSymbol"t";
     T := kk[t_0..t_14];
     TS := T**S;
     graph := substitute(vars T,TS)*substitute(omegaC,TS);          
          while(
     --time J := saturate(ideal graph, substitute(random(1,S),TS));
     J := saturate(ideal graph, substitute(random(1,S),TS));
     Ican := ideal mingens substitute(J,T);
     --counterCanEmb=counterCanEmb+1;
     not((dim Ican, genus Ican, degree Ican) == (2,15,28))) do();
     Ican)

--===========================================================================================================================--
--===========================================================================================================================--
--===============================	         DOCUMENTATION  	    	=============================================--
--===========================================================================================================================--
--===========================================================================================================================--

beginDocumentation()

document { 
  Key => RandomCurvesOverVerySmallFiniteFields,
  Headline => "randomly chosen smooth canonical curves over small finite fields",
  "This package can be seen as a refined version of the ",
  HREF("https://faculty.math.illinois.edu/Macaulay2/doc/Macaulay2-1.11/share/doc/Macaulay2/RandomCanonicalCurves/html/","RandomCanonicalCurves"),  
  " package, which catches all possible missteps in the constructions. 
  The construction follows the unirationality proof of M_g for g<=14 and the article ",
  HREF("http://arxiv.org/abs/1311.6962","Matrix factorizations and families of curves of genus 15"),
  " for the genus g=15 case. Since a unirational parametrization of M_g is only a rational map, bad choices of parameters
   in the construction might end up in the indeterminacy locus or other undesired subloci.
  Since for example a hypersurface in characteristic 2 contains about 90% of the F_2-rational points (see ", 
  HREF("http://arxiv.org/abs/math/0404342","A quick and dirty irreducibility Test for Multivariate Polynomials over F_q"),
  "), a failure of the construction in the various steps is quite likely. 
  We catch all possible missteps, and try again until success.",
  PARA{},
  "For g<=10 we construct the canonical curves via plane models.",
  PARA{},
   "For 10<g<14 the canonical curves are constructed via space models.",
  PARA{}, 
  "For g=14 the construction is based on Verra's proof of the unirationality of M_14 (see ", 
  HREF("http://arxiv.org/abs/math/0402032" ," The unirationality of the moduli space of curves of genus ≤14") ," 
  ).", 
  PARA{}, 
  "The g=15 construction relies on matrix factorizations and is based on the Macaulay2 Package ",
    HREF("http://arxiv.org/abs/1311.6962","Matrix factorizations and families of curves of genus 15"), 
  ".",
  PARA{}, 
  " For g <=14, the methods used in this package are based on the Macaulay2 Package ",
  HREF("http://www.math.uiuc.edu/Macaulay2/doc/Macaulay2-1.8.2/share/doc/Macaulay2/RandomCurves/html/","randomCurves"),
  "  and the methods for the g=15 case are based on the Macaulay2-package ",
    HREF("http://www.math.uni-sb.de/ag-schreyer/images/data/computeralgebra/M2/doc/Macaulay2/MatFac15/html/index.html","MatFac15"), 
  ".",
  Caveat => {"This package requires Macaulay2 Version 1.9 or newer."} 
   }

doc ///
  Key 
    isSmoothCurve
    (isSmoothCurve,Ideal) 
  Headline 
    Tests smoothness of a curve
  Usage
    isSmoothCurve(IC)
  Inputs
    IC: Ideal
       the Ideal of a curve       
  Outputs
      : Boolean
          , whether the curve is smooth or not      
  Description
     Text
       Checks whether a curve is smooth or not

     Example
         S = QQ[x,y,z];
	 IC = ideal(x*y);
	 isSmoothCurve(IC) 
	 IC2 = ideal (x^2+y^2+z^2);
	 isSmoothCurve(IC2)            
///

doc ///
  Key 
    smoothCanonicalCurve
    (smoothCanonicalCurve,ZZ,ZZ) 
    --[smoothCanonicalCurve,Printing]
  Headline 
    Computes the ideal of canonical curve
  Usage
    smoothCanonicalCurve(g,p)
  Inputs
    g: ZZ
       the genus
    p: ZZ
       a prime number defining the characteristic           
  Outputs
      ICan: Ideal
              the ideal of a (smooth) canonical curve of genus g over a field with characteristic p                      
  Description
     Text
       Computes a smooth canonical curve of genus g<=15 over a field of characteristc p.
       For genus g<=14 are based on the unirationality of M_g for g<=14 and the  RandomCurves-package.
       A unirational parametrization of M_g is only a rational map and bad choices of parameters 
       (which are quite likely over small fields) might end up in the indeterminacy locus or some
       other undesired subloci.
       In this constructions we catch the steps which do not work out for
       very small characteristic by catching all possible missteps. 
       
       For g<=10 the curves are constructed via plane models.
       
       For g<=13 the curves are constructed via space models. 
       
       For g=14 the curves are constructed by Verra's method.
       
       For g=15 the curves are constructed via matrix factorizations.
       
       If the option {\tt Printing} is set to {\tt true} then printings about the current step in the construction are displayed.
       
     Example
	 time ICan = smoothCanonicalCurve(11,5);
	 (dim ICan, genus ICan, degree ICan)
	 betti ICan
  SeeAlso
    smoothCanonicalCurveViaPlaneModel
    smoothCanonicalCurveViaSpaceModel
    smoothCanonicalCurveGenus14
    smoothCanonicalCurveGenus15	              
///

doc ///
  Key 
    smoothCanonicalCurveViaPlaneModel
    (smoothCanonicalCurveViaPlaneModel,ZZ,ZZ)
    --[smoothCanonicalCurveViaPlaneModel,Printing]  
  Headline 
    Computes the ideal of canonical curve via plane models
  Usage
    smoothCanonicalCurveViaPlaneModel(g,p)
  Inputs
    g: ZZ
       the genus
    p: ZZ
       a prime number defining the characteristic           
  Outputs
      ICan: Ideal
              the ideal of a (smooth) canonical curve of genus g over a field with characteristic p                      
  Description
     Text
       Computes a smooth canonical curve of genus g over a field of characteristc p. 
       The constructions are based on the unirationality proofs of M_g for g<=10 and the methods 
       in the RandomCurves-package.
       A unirational parametrization of M_g is only a rational map and bad choices of parameters 
       (which are quite likely over small fields) might end up in the indeterminacy locus or some
       other undesired subloci.
       In this constructions we catch the steps which do not work out for
       very small characteristic.
       The function works for g<=10.
       
       If the option {\tt Printing} is set to {\tt true} then printings about the current step in the construction are displayed.
  SeeAlso
    smoothCanonicalCurve
    smoothCanonicalCurveViaSpaceModel
    smoothCanonicalCurveGenus14
    smoothCanonicalCurveGenus15                   
///

doc ///
  Key 
    smoothCanonicalCurveViaSpaceModel
    (smoothCanonicalCurveViaSpaceModel,ZZ,ZZ)
    --[smoothCanonicalCurveViaSpaceModel,Printing] 
  Headline 
    Computes the ideal of canonical curve via space models
  Usage
    smoothCanonicalCurveViaSpaceModel(g,p)
  Inputs
    g: ZZ
       the genus
    p: ZZ
       a prime number defining the characteristic         
  Outputs
      ICan: Ideal
              the ideal of a (smooth) canonical curve of genus g over a field with characteristic p                      
  Description
     Text
       Computes a smooth canonical curve of genus g over a field of characteristc p. 
       The constructions are based on the unirationality proofs of M_g for g<=10 and the methods 
       in the RandomCurves-package.
       A unirational parametrization of M_g is only a rational map and bad choices of parameters 
       (which are quite likely over small fields) might end up in the indeterminacy locus or some
       other undesired subloci.
       In this constructions we catch the steps which do not work out for
       very small characteristic.
       The Function works for 11<=g<=13.
       
       If the option {\tt Printing} is set to {\tt true} then printings about the current step in the construction are displayed.
  SeeAlso
    smoothCanonicalCurve
    smoothCanonicalCurveViaPlaneModel
    smoothCanonicalCurveGenus14
    smoothCanonicalCurveGenus15                   
///

doc ///
  Key 
    smoothCanonicalCurveGenus15
    (smoothCanonicalCurveGenus15,ZZ) 
    --[smoothCanonicalCurveGenus15,Printing] 
  Headline 
    Computes the ideal of canonical curve of genus 15
  Usage
    smoothCanonicalCurveGenus15(p)
  Inputs
    p: ZZ
       the characteristic         
  Outputs
      ICan: Ideal
              the ideal of a (smooth) canonical curve of genus g over a field with characteristic p                      
  Description
     Text
       Computes a smooth canonical curve of genus g over a field of characteristc p. The construction uses matrixfactorizations
       and we catch the steps which do not work out for very small characteristic. 
       The whole construction is based on the Macaulay2 package "MatFac15"
       
       If the option {\tt Printing} is set to {\tt true} then printings about the current step in the construction are displayed.
  SeeAlso
    smoothCanonicalCurve
    smoothCanonicalCurveViaPlaneModel
    smoothCanonicalCurveViaSpaceModel
    smoothCanonicalCurveGenus14                  
///

doc ///
  Key
    smoothCanonicalCurveGenus14
    (smoothCanonicalCurveGenus14,ZZ)
    --[smoothCanonicalCurveGenus14,Printing]
  Headline
    Compute a random canonical curve of genus 14
  Usage
    smoothCanonicalCurveGenus14(p)
  Inputs
    p: ZZ
       a prime number defining the characteristic
  Outputs
    ICan: Ideal 
        the ideal of a smooth canonically embedded genus 14 curve  
  Description
    Text 
      Computes a smooth canonical curve of genus 14 over a field of characteristc p. 
       The constructions are based on the unirationality proof of M_14 by A. Verra (See "http://arxiv.org/abs/math/0402032")
       and the methods in the Macaulay2-Package "RandomGenus14Curves".
       A unirational parametrization of M_g is only a rational map and bad choices of parameters 
       (which are quite likely over small fields) might end up in the indeterminacy locus or some
       other undesired subloci.
       In this constructions we catch the steps which do not work out for
       very small characteristic.
       
       If the option {\tt Printing} is set to {\tt true} then printings about the current step in the construction are displayed.
  SeeAlso
    smoothCanonicalCurve
    smoothCanonicalCurveViaPlaneModel
    smoothCanonicalCurveViaSpaceModel  
    smoothCanonicalCurveGenus15          
/// 

doc ///
     Key
          Printing
          [smoothCanonicalCurve, Printing]
	  [smoothCanonicalCurveViaPlaneModel, Printing]
	  [smoothCanonicalCurveViaSpaceModel, Printing]
	  [smoothCanonicalCurveGenus14, Printing]
	  [smoothCanonicalCurveGenus15, Printing]
     Headline
          displays information about the current step in the constructions. Default value is "false".
     Description
          Text
	       Setting the option {\tt Printings} to {\tt true} displays information about step in the construction which is currently performed.
	       The default value for all functions is {\tt false}.
	       
	       This might be helpful in order to check how often a particular construction step fails in small characteristic.
  SeeAlso
    smoothCanonicalCurve
    smoothCanonicalCurveViaPlaneModel
    smoothCanonicalCurveViaSpaceModel  
    smoothCanonicalCurveGenus14
    smoothCanonicalCurveGenus15  
///

doc ///
     Key
      Details
      [smoothCanonicalCurve, Details]
      [smoothCanonicalCurveGenus14, Details]
      [smoothCanonicalCurveGenus15, Details]
     Headline
          keeps track of the precise step in the construction of canonical genus 14 and 15 curves. Default value is "false".       
     Description
          Text
	       Setting the option {\tt Details} to {\tt true} displays the precise step in the construction which is currently performed.
	       This option displays additional information compared to the option {\tt Printing}.
	       The purpose of this additional option is to further test and tweak the genus 14 and 15 construction.
	       The output displayed uses the notation from the sourcecode. Thus, without having a look into the sourcecode this option might not be helpful.
	  
	       The default value for all functions is {\tt false}.
	       
	       Enabling this option also enable the option {\tt Printing}.
  SeeAlso
    Printing
    smoothCanonicalCurve 
    smoothCanonicalCurveGenus14
    smoothCanonicalCurveGenus15  
///
end

restart
uninstallPackage("RandomCurvesOverVerySmallFiniteFields")
restart
installPackage("RandomCurvesOverVerySmallFiniteFields", RerunExamples=>false)
viewHelp
--loadPackage("RandomCurvesOverVerySmallFiniteFields")

TEST ///

time I=smoothCanonicalCurve(11,2);
time I=smoothCanonicalCurve(11,3);
time I=smoothCanonicalCurve(11,5);
time I=smoothCanonicalCurve(11,101); -- used 7.73312 seconds




///
