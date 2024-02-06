-- -*- coding: utf-8 -*-
newPackage(
   "HodgeIntegrals",
   Version => "1.2.1",
   Date => "29 April 2010",
   Certification => {
	"journal name" => "The Journal of Software for Algebra and Geometry: Macaulay2",
	"journal URI" => "http://j-sag.org/",
	"article title" => "Intersection numbers on Mbar_{g,n}",
	"acceptance date" => "2010-04-17",
	"published article URI" => "http://j-sag.org/Volume2/jsag-1-2010.pdf",
	"published code URI" => "http://j-sag.org/Volume2/HodgeIntegrals.m2",
	"repository code URI" => "https://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/HodgeIntegrals.m2",
	"release at publication" => "5169e42924f6675b7eec36596b3eeafd8b718141",
	"version at publication" => "1.2.1",
	"volume number" => "2",
	"volume URI" => "http://j-sag.org/Volume2/"
	},
   Authors => {
	 {  Name => "Stephanie Yang",
	    Email => "stpyang@math.kth.se",
	    HomePage => "http://www.stephanieyang.com"}
         },
   Headline => "Hodge integrals on the moduli space of curves",
   Keywords => {"Commutative Algebra"},
   DebuggingMode => false
   )

   export {"hodgeRing", "wittenTau", "integral", "kappa", "lambda", "psi", "ch"}

------------------------------------------------------------------------

-- extends the factorial function to a List of ZZ
List ! := a -> product(a, r -> r!)

-- extends the binomial function to Lists of ZZ

--OLD CODE
--binomial (List, List) := ZZ => (n, i) -> (
--     if #n =!= #i then error "expected lists with the same length";
--     product(#n, j -> binomial(n#j, i#j))
--     )

--BETTER CODE
binomial (List, List) := ZZ => (n, i) -> product(n, i, binomial)

-- these two functions allow one to convert between the following
-- expressions for exponents of tau and exponents of psi:
-- <tau_m#1 tau_m#2 ... tau_m#n> = int_M(g, n) psi_1^m#1 ... psi^m#n
--               = < tau_0^a#0 tau_1^a#1 tau_2^a#2 ... >
exponentsToTauList = a -> flatten apply(#a, i -> toList(a#i:i));
tauListToExponents = m -> (
     T := tally m;
     apply(1 + max keys T, i -> if T#?i then T#i else 0)
     )	 

-- computes all lists of integers which are componentwise at most "a";
-- this function is used to compute the binomial convolution of the
-- wittenTau's, or equivalently the product of the exponential generating
-- functions.
splittings = memoize (
     a -> if #a === 1 then apply(1 + a#0, i -> {i})
     else flatten apply(1 + a#0, i ->
	  apply(splittings drop(a, 1), l -> {i}|l)
	  )
     )

-- creates the list corresponding to the i - th standard basis vector in
-- RR^k
ee = memoize ( (i, k) -> apply(k, j -> if j === i then 1 else 0) )


-- computes coefficients in Witten's exponential generating function,
-- specifically, wittenTau(a) := < tau_0^a#0 tau_1^a#1 tau_2^a#2 ...>
-- (using the formula of Liu-Xu)
wittenTau = method();
wittenTau List := QQ => ( memoize (
     a -> if any(a, x -> x < 0) then 0/1 else (
     	  -- eliminate trailing zeros in the input list "a"
     	  h := 3 + sum(#a, i -> (i-1)*a#i);   
     	  -- initial conditions
     	  if h % 3 != 0 then 0/1
          else wittenTau(h//3,a)
     	  )
     )
)

wittenTau (ZZ,List) := QQ => ( memoize (
     (g,a) -> if any(a, x -> x < 0) then 0/1 else (
     	  -- eliminate trailing zeros in the input list "a"
     	  p := position(a, x -> x > 0, Reverse => true);
     	  if p === null then k := 0 else k = p+1;
     	  a = take(a,k);  
     	  -- determine the moduli space parameters
     	  n := sum a;
	  -- check that the coefficient is nonzero
	  if 3*g-3+n != sum(#a, i-> i*a#i) then 0/1
     	  -- initial conditions   
     	  else if k === 0 or g<0 then 0/1
     	  else if k === 1 then (
	       if a#0 === 3 then 1/1 else 0/1)
	  else if g === 0 then (n-3)! / (exponentsToTauList(a))!
	  else if #select(toList(0..k-2), i -> a#(i+1) > 0) === 1 and a#(k-1) === 1 then 1/((24^g)*g!)
          else liuXuCoeff(g,n,k,a)
     	  )
     )
)


-- NOTE: keep this function *separate* from the function wittenTau
-- because we are following the algorithm presented in the Liu-Xu
-- paper, and this may change in future versions.
liuXuCoeff = memoize (
     (g, n, k, a) -> (
	  --dilaton equation
   	  if a#1 > 0 then (2 * g - 3 + n) * wittenTau(a - ee(1, k))
	  --string equation
   	  else if a#0 > 0 then sum(k - 1, i -> a#(i + 1) *
	       wittenTau(a - ee(0, k) + ee(i, k) - ee(i + 1, k)))
   	  --KdV equation plus string equation
   	  else (
	       d := position(drop(a, 1), x -> x>0) + 1;
	       a' := (a|{0}) - ee(d, k + 1);
               tempCoeff := (
		    (2*d + 3)/12 *
	       	    wittenTau(g - 1, 4*ee(0, k + 1) + ee(d + 1, k + 1) + a') -
	       	    (2*g + n - 1)/6 *
	       	    wittenTau(g - 1, 3*ee(0, k) + a) +
	       	    sum(splittings(a'), b -> binomial(a', b) * (
			      (2*d + 3) *
			      wittenTau(2*ee(0, k + 1) + ee(d + 1, k + 1) + b) *
			      wittenTau(2*ee(0, k + 1) + a' - b) -
			      (2*g + n - 1) *
			      wittenTau(ee(0, k + 1) + ee(d, k + 1) + b) *
			      wittenTau(2*ee(0, k + 1) + a' - b)
			      )
     			 )
		    );
	       return (tempCoeff/(2*g + n - 1)/(2*g + n - 2))
	       )
	  )
     )

------------------------------------------------------------------------

-- define kappa, lambda, psi, ch
kappa = new IndexedVariableTable
lambda = new IndexedVariableTable
psi = new IndexedVariableTable
ch = new IndexedVariableTable


-- establish a big Ring where all calculations take place
hodgeRing = (g,n) -> (
     MAXg  := g;
     MAXn  := 4*g + n - 3;
     MAXkappa := 3*MAXg - 3 + MAXn;
     MAXlambda := MAXg;
     MAXpsi := MAXn;
     MAXch := 2*MAXg - 1;
     QQ[kappa_1..kappa_MAXkappa, lambda_1..lambda_MAXlambda, psi_1..psi_MAXpsi, ch_1..ch_MAXch,
     Degrees => toList splice (1..MAXkappa, 1..MAXlambda, MAXpsi:1, 1..MAXch)]
     )


-- multiply the frequencies of all elements of a tally by an integer n
--  original code
--  ZZ * Tally := (n, T) -> new Tally from apply(keys T, x -> (x => (n * T#x)))
-- Here is a better suggestion
ZZ * Tally := (n,T) -> if n == 0 then new Tally else applyValues(T, x->n*x)

-- convert from a List of subscripts or superscript to a monomial
-- <a#0 a#1 .. a#n> -> psi_1^a#0 psi_2^a#1 .. psi_(n + 1)^a#n
-- <a#0 a#1 .. a#n> -> kappa_1^a#0 kappa_2^a#1 .. kappa_(n + 1)^a#n
-- NOTE: beware the shift by 1 in the subscripts
psiListToProduct = psiList -> product(#psiList, i -> psi_(i + 1)^(psiList#i))

kappaListToProduct = kappaList -> product(#kappaList, i -> kappa_(i + 1)^(kappaList#i))


-- calculate Bernoulli numbers
bernoulli = memoize (
     n -> if n === 0 then 1 else - sum(n, i -> binomial(n + 1, i) * bernoulli(i)/(n + 1))
     )


-- check to see if the codimension of the tautological class
-- equals the dimension of the moduli space
dimCheck = memoize (
     (g, n, chKaTauSeq) -> (
	  chDim := sum(#chKaTauSeq#0, i -> (i + 1) * chKaTauSeq#0#i);
	  kappaDim := sum(#chKaTauSeq#1, i -> (i + 1) * chKaTauSeq#1#i);
	  tauDim := sum(chKaTauSeq#2);
	  3*g - 3 + n === chDim + kappaDim + tauDim)
     )


-- convert la_n to an expression in ch_i using the formula
-- sum(1..n, i -> lambda_i) = exp sum((n + 1)//2, i -> (2 * i)! * ch_(2 * i + 1));
-- NOTE: according to Mumford's paper, ch_(2 * i)=0
--
--oldlambdaToCh = memoize (
--     (n, R) -> (
--  	  ChRing := QQ[ch_1..ch_n, Degrees=>{1..n}];
--  	  I := ideal basis(n + 1, 2 * n - 1, ChRing);
--  	  ChRing = ChRing/I;
--  	  f := sum((n + 1)//2, i -> (2 * i)! * ch_(2 * i + 1));
--  	  sub(part(n, sum(1..n, i -> (1/i!) * f^i)), R)
--   	  )
--     )

lambdaToCh = memoize (
     (n, R) -> (
  	  tempChRing := QQ[local tempCh_1..local tempCh_n, Degrees=>{1..n}];
  	  I := ideal basis(n + 1, 2 * n - 1, tempChRing);
  	  tempChRing = tempChRing/I;
  	  x := sum((n + 1)//2, i -> (2 * i)! * tempCh_(2 * i + 1));
  	  x = part(n, sum(1..n, i -> (1/i!) * x^i));
          F := map(R,tempChRing,{ch_1..ch_n});
	  F x
   	  )
     )


-- calculate all of the codimension c boundary divisors on M_{g, n}
-- that appear in the Riemann-Roch formula
deltaSet = memoize (
     (g, n, c) -> (
  	  N := set(0..n - 1);
  	  dT := set (0..g) ** set subsets n;
  	  dT = dT - set{0} ** set subsets (n, 0) - set{0} ** set subsets (n, 1);
  	  dT = dT - set{g} ** set subsets (n, n - 1) - set{g} ** set subsets (n, n);
  	  dT = (dT ** set apply(0..c - 1, i -> (i, c - 1 - i))) / splice;
  	  dT = dT / (x -> {(x#0, set x#1, {x#2}), (g - x#0, N - x#1, {x#3})});
  	  dT = dT + set apply(0..c - 1, i -> {(g - 1, N, {i, c - 1 - i})})
  	  )
     )


-- pull back a product of kappa and ch classes to
-- a reducible codimension one or two topological degeneration
splitChKa = memoize (
     chKaList -> (
  	  tempTally := splitChKa(drop(chKaList, 1));
  	  a := first chKaList;
  	  sum(0..a, i -> binomial(a, i) * tempTally / (x -> {{i}|x_0, {a - i}|x_1}))
  	  ),
     {{} => tally {{{}, {}}}}
     )

chKaTally = memoize (
     (chList, kappaList) -> (splitChKa(chList) ** splitChKa(kappaList))/splice)
   

-- compute a tally of all pairs of kappaLists and chLists that arise
-- from a reducible codimension one or two topological degeneration
split = method();
split List := Tally => memoize (
     expList -> if expList === {} then tally {{{}, {}}} else (
	  tmpTally := split drop(expList, 1);
	  a := first expList;
	  sum(a + 1, i -> binomial(a, i) * tmpTally / (x -> {{i} | x_0, {a - i} | x_1})))
     )

split (List, List) := Tally => memoize (
     (chList, kappaList) -> (split(chList) ** split(kappaList)) / toList
     )


-- this append the elements of tauList, indexed by, N to psiList
-- (this is used to calculate codimension one degenerations of pointed curves)
nTauPsi = memoize (
     (N, tauList, psiList) -> apply(toList N, i -> tauList#i) | psiList
     )


-- computes the product of a boundary divisor with
-- a monomial in ch, kappa, or psi classes
dProduct = memoize (
     (delta, chKaTauSeq, KaLaPsChRing) -> (
  	  (g1, N1, p1) := delta#0;
  	  (chList, kappaList, tauList) := chKaTauSeq;
	  -- singular irreducible case
  	  if #delta === 1 then
   	  return (-1)^(p1#0) * klp(g1, #N1 + 2, (chList, kappaList, tauList|p1), KaLaPsChRing)
	  --
  	  else (
   	       (g2, N2, p2) := delta#1;
	       -- exploit symmetry for speed
	       if (g1>g2) or (g1===g2 and member(0, N2)) then (
		    return dProduct(reverse delta, chKaTauSeq, KaLaPsChRing)
		    );
	       ckTally := chKaTally(chList, kappaList);
	       return sum (keys ckTally / (x -> (
			      if dimCheck(g1, #N1 + 1, (x#0#0, x#1#0, nTauPsi(N1, tauList, p1)))
			      then (
			      	   (-1)^(p1#0) * ckTally#x *
			      	   klp(g1, #N1 + 1, (x#0#0, x#1#0, nTauPsi(N1, tauList, p1)), KaLaPsChRing) *
			      	   klp(g2, #N2 + 1, (x#0#1, x#1#1, nTauPsi(N2, tauList, p2)), KaLaPsChRing)
     				   )
			      else 0
			      )
		     	 )
		    )
	       )
      	  )	
     )


deltaProduct = memoize (
     (delta, chKaTauSeq) -> (
	  (g0, N0, p0) := delta#0;
	  (chList, kappaList, tauList) := chKaTauSeq;
	  if #delta === 1 then (-1)^(p0#0) * integral(g0, 2 + #N0, (chList, kappaList, tauList | p0))
	  else (
	       (g1, N1, p1) := delta#1;
	       tmpTally := split(chList, kappaList);
	       sum(keys tmpTally, d -> (
		      	 if dimCheck(g0, #N0 + 1, (d#0#0, d#1#0, nTauPsi(N0, tauList, p0)))
		      	 then (-1)^(p0#0) * tmpTally#d * (
		      	      integral(g0, #N0 + 1, (d#0#0, d#1#0, nTauPsi(N0, tauList, p0))) *
		      	      integral(g1, #N1 + 1, (d#0#1, d#1#1, nTauPsi(N1, tauList, p1)))
		      	      )
		      	 else 0
		      	 )
	    	    )
       	       )
       	  )
     )


-- THE MAIN FUNCTION
integral = (g, n, klpc) -> (
   if (g < 0) or (3 * g - 3 + n < 0) then 0
   else if (klpc == 1) and (3 * g - 3 + n == 0) then 1
   else if (klpc == 1) and (3 * g - 3 + n != 0) then 0
   else (
	-- Set MAX parameters
        KaLaPsChRing := ring (klpc);
	MAXkappa := index lambda_1;
	MAXlambda := index psi_1 - index lambda_1;
	MAXpsi := index ch_1 - index psi_1;
	MAXch := 2*MAXlambda - 1;
	-- add dimension check and warning when the codim of the class != dim of ring
	-- eliminate lambdas and unnecessary other stuff
	kappaImg := splice (kappa_1..kappa_(3 * g - 3 + n), MAXkappa - (3 * g - 3 + n):0);
	lambdaImg := splice (apply(1..g, i -> lambdaToCh (i, KaLaPsChRing)), MAXlambda - g:0);
	psiImg := splice (psi_1..psi_n, MAXpsi - n:0);
	chImg := splice (ch_1..ch_(2 * g - 1), min(MAXch, MAXch - (2 * g - 1)):0);
	--chImg = apply(chImg, x -> sub(x, KaLaPsChRing));
	tempMap := map(KaLaPsChRing, KaLaPsChRing, toList (kappaImg|lambdaImg|psiImg|chImg));
	ckpPoly := tempMap klpc;
	monList := flatten entries (coefficients ckpPoly)_0;
	coeffList := flatten entries (coefficients ckpPoly)_1;
	sum(#terms ckpPoly, i -> (
		  chList := (first exponents monList_i)_{
		       MAXkappa + MAXlambda + MAXpsi..(MAXkappa + MAXlambda + MAXpsi + 2 * g - 1 - 1)};
		  kappaList := (first exponents monList_i)_{0..3 * g - 3 + n - 1};
		  tauList := (first exponents monList_i)_{
		       (MAXkappa + MAXlambda)..(MAXkappa + MAXlambda + n - 1)
		       };
		  coeffList_i * klp(g, n, (chList, kappaList, tauList), KaLaPsChRing)
		  )
	     )
	)
   )

-- inputs the list {g, n, ch, kappa, psi} and computes the klp integral
-- int_M(g, n) chern_0^ch#0 chern_1^ch#1 .. chern_a^ch#a
--             kappa_0^kappa#0 kappa_1^kappa#1 .. kappa_b^kappa#b
--                 psi_1^p#1 psi_2^p#2     .. psi_n^p#n
klp = memoize (
     (g, n, chKaTauSeq, KaLaPsChRing) -> (
          -- Set MAX parameters
          MAXkappa := index lambda_1;
	  MAXlambda := index psi_1 - index lambda_1;
	  MAXpsi := index sub(ch_1,KaLaPsChRing) - index psi_1;
	  MAXch := 2*MAXlambda - 1;
  	  -- define g and n	
  	  if (g < 0) or (3 * g - 3 + n < 0) then return 0;
  	  if not dimCheck(g, n, chKaTauSeq) then return 0;
  	  (chList, kappaList, tauList) := chKaTauSeq;
  	  if not all(tauList, x -> x >= 0) then return 0;
  	  if tauList =!= (sort tauList) then return klp(g, n, (chList, kappaList, sort tauList), KaLaPsChRing);
	  -- dan abramovich's shortcut
	  chDim := sum(#chList, i -> (i + 1) * chList#i);
	  if g===0 then (if chDim>0 then return 0;)
	  else if (g===1) then (if chDim>1 then return 0;)
	  else if chDim>3 * g - 3 then return 0;
	  if g>1 and (sum(#chList, i -> (i + 1) * chList#i) > (3 * g - 3)) then return 0;
  	  -- define chList length c
  	  p := position(chList, r -> r > 0, Reverse => true);
  	  if p === null then c := 0 else c = p + 1;
  	  chList = take(chList, c);
  	  if c > 2 * g then return 0;
  	  --define kappaList length k
  	  p = position(kappaList, r -> r > 0, Reverse => true);
  	  if p === null then k := 0 else k = p + 1;
  	  kappaList = take(kappaList, k);
  	  -- define tauList and set length to n
  	  tauList = tauList | splice{n - length(tauList):0};
  	  -- eliminate ch_c using Riemann-Roch formula (cf. Mumford's paper)
  	  if c > 0 then (
   	       newChList := chList - ee(c - 1, c);
   	       (kappaList, k) = (kappaList|splice{c - k:0}, max(c, k));
   	       val := klp(g, n, (newChList, kappaList + ee(c - 1, k), tauList), KaLaPsChRing);
   	       val = val - sum(n, i -> klp(g, n, (newChList, kappaList, tauList + c * ee(i, n)), KaLaPsChRing));
   	       dSet := deltaSet(g, n, c);
   	       dSet = keys dSet / (x -> dProduct(x, (newChList, kappaList, tauList), KaLaPsChRing));
   	       val = val + 1/2 * sum dSet;
   	       return bernoulli(c + 1)/(c + 1)! * val;
  	       )
  	  -- eliminate kappa_k using push-pull formula
  	  else if k > 0 then (
   	       kappaImg := splice(apply(1..k, i -> kappa_i - psi_(n + 1)^i), MAXkappa - k:0);
   	       lambdaImg := splice(MAXlambda:0);
   	       psiImg := splice (psi_1..psi_n, MAXpsi - n:0);
   	       tempMap := map(KaLaPsChRing, KaLaPsChRing, {splice kappaImg, lambdaImg, psiImg, MAXch:0});
   	       kpmon := kappaListToProduct (kappaList - ee(k - 1, k)) * psiListToProduct(tauList);
   	       return integral(g, n + 1, psi_(n + 1)^(k + 1) * tempMap kpmon);
   	       )
  	  -- call Liu-Xu's function
  	  else (
	       return wittenTau(tauListToExponents(tauList));
	       )
 	  ),
     --base case integral(0,3,1)=1, since M_{0,3} is a point
     {(0, 3, {{0}, {0}, {0, 0}})=>1}
     )

beginDocumentation()

document {
     Key => {HodgeIntegrals},
     Headline => "Hodge integrals on the moduli space of curves",
     PARA {
       	  TO "HodgeIntegrals",
       	  TEX ///
	  is a package for evaluating intersection numbers on the Deligne-Mumford moduli space of $n$-pointed stable curves of genus $g$, often denoted ${\bar M}_{g,n}$.
	  This package evaluates integrals of the form
	  $$\int_{{\bar M}_{g,n}} \psi_1^{e_1} ... \psi_n^{e_n} k_1^{f_1} ... k_b^{f_b} \lambda_1^{h_1} ... \lambda_g^{h_g},$$
       	  where the values of $\psi_i$, $k_i$, and $\lambda_i$ are defined as follows:
	  ///,
	  },
     UL {
	  {
	       TEX ///
	       $\psi_i$ is the first Chern class of the $i$-th cotangent line bundle $L_i$,
	       whose value at a fixed curve $(C; p_1,...,p_n)$ is the cotangent space to $C$ at $p_i$.
	       ///,
	       },
	  {
	       TEX ///
	       $k_j$ is the pushforward of $\psi_i^{j+1}$ via the forgetful morphism which forgets the $i$-th marked point.
	       ///,
	       },
	  {
	       TEX ///
	       $\lambda_i$ is the $i$-th Chern class of the Hodge bundle $E$,
	       whose value at a fixed curve $(C; p_1,...,p_n)$ is $H^0(C,K_C)$,
	       or the space of differential one-forms on $C$.
	       ///,
	       },
	  },
     PARA {
	  TEX ///
	  A good introduction to ${\bar M}_{g,n}$ and related spaces can be found in the textbook [HM].
	  Two good references for the algebraic classes $\psi_i$, $k_i$, and $\lambda_i$,
	  as well as their properties, are [AC] and [M].
	  ///,
	  },
     PARA {
	  "This package is modelled after Carel Faber's Maple program ",
	  TT "KaLaPs",
	  ", available for download [F]. For more details on how this package works, please read [Y].",
	  },
     SUBSECTION "References",
     PARA {
	  "[AC] ",
	  "Arbarello, E. and Cornalba, M. ",
	  EM "Combinatorial and algebro-geometric cohomology classes on the moduli spaces of curves",
	  ". J. Algebraic Geom. 5. (1996), no. 4, 705--749."
	  },
     PARA {
	  "[F] ",
	  "Faber, Carel. ",
	  "Maple program for calculating intersection numbers on moduli spaces of curves. Available at ",
	  HREF "http://math.stanford.edu/~vakil/programs/index.html",
	  ".",
	  },
     PARA {
	  "[HM] ",
	  "Harris J., and Morrison, I. ",
	  EM "Moduli of Curves",
	  ", Graduate Texts in Mathematics 187. Springer-Verlag, New York, 1996. ISBN: 0387984291."
	  },   
     PARA {
	  "[V] ",
	  "Vakil, R. ",
	  EM "The moduli space of curves and Gromov-Witten theory",
	  ". Enumerative invariants in algebraic geometry and string theory (Behrend and Manetti eds.), Lecture Notes in Mathematics 1947, Springer, Berlin, 2008."
	  },
     PARA {
	  "[Y] ",
	  "Yang, S., ",
	  EM "Intersection numbers on ",
	  TEX ///${\bar M}_{g,n}$///,
	  ".",
	  },
     SUBSECTION "Contributors",
     PARA {
	  "The following person has generously contributed code or worked on our code.",
	  },
     UL {
	  HREF {"http://www.mast.queensu.ca/~ggsmith/","Greg Smith"},
	  },
     }
     
document {
     Key => {hodgeRing},
     Headline => "create a ring containing algebraic classes on moduli spaces of curves",
     Usage => "hodgeRing(g,n)",
     Inputs => {"g" => ZZ, "n" => ZZ},
     Outputs => {PolynomialRing},
     PARA {
          "The function ",
	  TO "hodgeRing",
	  " must must be called before ",
	  TO "integral",
	  " in order to initialize a ring ",
	  TT "QQ[",
          TEX ///$\psi_1, ..., \psi_a, k_1, ..., k_b, \lambda_1, ..., \lambda_c$///,
          TT "]",
	  " containing variables used by ",
	  TO "integral",
	  ".  The inputs ",
          EM "g",
          " and ",
          EM "n",
          " should be at least as large as the genus and number of points that will used. Overestimating the values of ",
          EM "g",
	  " and ",
	  EM "n",
	  " are fine, but initializing these numbers too small will result in error messages."
       	  },
     SUBSECTION "Caveat",
     PARA {
     	  "The output of ",
	  TT "hodgeRing",
	  " is not a geometric object but a computational one. ",
	  "The intersection numbers are calculated recursively using pullbacks by natural morphisms (c.f., equations (4), (8)--(11), and (13) of [Y]). ",
	  "Rather than initializing a new tautological ring for every step of this recursion, this package provides the function hodgeRing to the user to create a ring large enough to contain all the variables which might be needed, and uses endomorphisms of the master ring instead of natural morphisms between several rings."
	  },
     PARA {
       	  "Here are some examples: ",
     	  },
     EXAMPLE {
     	  "R = hodgeRing (4, 1);",
     	  "integral (1, 1, psi_1)",
     	  "integral (3, 0, lambda_1^6)",
     },
     SUBSECTION "References",
     PARA {
	  "[Y] ",
	  "Yang , S.",
	  EM "Intersection numbers on ",
	  TEX ///${\bar M}_{g,n}$///,
	  ".",
	  },
     SeeAlso => {"HodgeIntegrals", "integral"}
     }

document {
     Key => {integral},
     Headline => "evaluate Hodge integrals",
     Usage => "integral(g, n, klp)",
     Inputs => {"g" => ZZ, "n"=> ZZ},
     Outputs => {QQ},
     PARA {
     	  TEX ///
	  This function computes top intersection numbers among tautological classes on the moduli space of curves.
	  The tautological classes include products of the Mumford-Morita-Miller classes $k_i$,
	  the cotangent line classes $\psi_i$, and the Chern classes and Chern characters, $\lambda_i$ and $ch_i$ of the Hodge bundle.
	  ///,
	  },
     PARA {
	  "The function ",
	  TO "hodgeRing",
	  " must be called previously with values of ",
	  TT "g",
	  " and ",
	  TT "n",
	  " at least as large as those to be used.,"
	  },
     SUBSECTION "Examples",
     	  PARA {
	  TEX ///
	  Here are a few examples illustrating the $\lambda_g$ formula [FP, Theorem 1],
	  $$\int_{{\bar M}_{g,n}} \psi_1^{a_1}...\psi_n^{a_n} \lambda_g= |B_{2g}|(2g+n-3)!(2^{2g-1}-1) / (a_1!...a_n!2^{2g-1}(2g)!),$$
	  where $B_i$ represents the $i$-th Bernoulli number.
	  ///,
	  },
     EXAMPLE {
     	  "R = hodgeRing (3, 3);",
     	  "integral (1, 1, lambda_1)",
     	  "integral (2, 2, psi_1 * psi_2^2 * lambda_2)",
     	  "integral (3, 3, psi_1 * psi_2^2 * psi_3^3 * lambda_3)",
     },
     PARA {
	  "Here are a few more examples.",
	  },
     EXAMPLE {
     	  "R = hodgeRing (4, 0);",
     	  "integral (2, 0, lambda_1^3)",
     	  "integral (3, 0, lambda_1^6)",
     	  "integral (4, 0, lambda_1^9)",
     },
     SUBSECTION "References",
     PARA {
	  "[FP] ",
	  "Faber, C. and Pandharipande, R., ",
	  EM "Hodge integrals, partition matrices, and the ",
	  TEX ///$\lambda_g$///,
	  EM " conjecture. ",
	  "Annals of Mathematics, 156 (2002), 97-124.",
	  },
     SeeAlso => {"HodgeIntegrals","hodgeRing","wittenTau"}
     }


document {
     Key => {wittenTau,
	  (wittenTau,List),
	  (wittenTau,ZZ,List)},
     Headline => "Witten tau integrals",
     Usage => "wittenTau(g,a), wittenTau(a)",
     Inputs => {"g" => ZZ, "a" => List},
     Outputs => {QQ},
     PARA {
	  TEX ///
	  The Witten tau coefficients are top intersection numbers of cotangent line classes on the moduli space of curves.
	  The integral of $\psi_1^{d_1}\psi_2^{d_2}...\psi_n^{d_n}$ on the moduli space of stable $n$-pointed curves of genus $g$ is denoted:
      	  $$\int_{{\bar M}_{g,n}} \psi_1^{d_1}...\psi_n^{d_n} = <\tau_{d_0}\tau_{d_1}...\tau_{d_n}> = <\tau_0^{a_0}\tau_1^{a_1}...\tau_k^{a_k}>.$$
          The list $\{a_0,a_1,...,a_k\}$ is the argument for
	  ///,
	  TT "wittenTau",
	  ". These integrals are computed recursively using the string equation, dilation equation, and an effective genus recursion formula of Liu and Xu [LX].",
	  },	
     PARA {     
     	  "The genus is an optional parameter. If it is omitted, the genus is automatically calculated.",
     	  },
     SUBSECTION "Examples",
     PARA {
	  "Here are some examples illustrating the well-known formula that is a result of Witten's conjecture:",
	  TEX ///
	  $$\int_{{\bar M}_{0,n}} \psi_1^{a_1}...\psi_n^{a_n} = \frac{(n-3)!}{a_1!...a_n!}$$
	  ///,
	  },
     EXAMPLE{
	  "wittenTau (0,{3})",
	  "wittenTau (0,{4, 1, 1})",
	  "wittenTau (0,{5, 0, 2})",
	  },	 
     PARA {
    	  "Here are some additional examples in higher genus.",
	  },
     EXAMPLE{
	  "wittenTau (1,{0,1})",
	  "wittenTau (3,{0,0,0,0,0,1})",
	  "wittenTau (5,{0,0,0,0,0,3})",
	  },	 
     SUBSECTION "References",
     PARA {
	  "[LX] ",
	  "Liu, K. and Xu, H. ",
	  EM "An effective recursion formula for computing intersection numbers",
	  ". Available at ",
	  HREF "http://front.math.ucdavis.edu/0710.5322",
	  },
     SeeAlso => {"HodgeIntegrals","hodgeRing","integral"}
     }

document {
     Key => {ch},
     Headline => "Chern character of the Hodge bundle",
     Usage => "ch_a",
     Inputs => { "a" => ZZ },
     Outputs => { RingElement },
     "This is an element in the ring created by ",
     TT "hodgeRing",
     ". ",
     TEX ///
     It is the $a$-th graded part of the Chern character of the Hodge bundle on ${\bar M}_{g,n}$.
     ///,
     SUBSECTION "Examples",
     PARA {
	  TEX ///
	  Here is a simple example which calculates $\int_{{\bar M}_{1,1}} ch_1$.
	  ///,
	  },
     EXAMPLE{
	  "R = hodgeRing (1, 1);",
	  "ch_1",
	  "integral(1, 1, ch_1)",
	  },	 
     SeeAlso => {"HodgeIntegrals","hodgeRing","integral"}
     }

document {
     Key => {kappa},
     Headline => "Miller-Morita-Mumford classes",
     Usage => "kappa_a",
     Inputs => { "a" => ZZ },
     Outputs => { RingElement },
     "This is an element in the ring created by ",
     TT "hodgeRing",
     ". It is the Miller-Morita-Mumford class discussed in [AC].",
     PARA {
	  TEX ///
	  Here is a simple example which calculates $\int_{{\bar M}_{1,1}} k_1$.
     	  ///,
	  },
     EXAMPLE {
	  "R = hodgeRing (1, 1);",
	  "kappa_1",
	  "integral(1, 1, kappa_1)",
	  },	 
     SUBSECTION "References",
     PARA {
	  "[AC] ",
	  "Arbarello, E. and Cornalba, M. ",
	  EM "Combinatorial and algebro-geometric cohomology classes on the moduli spaces of curves",
	  ". J. Algebraic Geom. 5. (1996), no. 4, 705--749."
	  },
     SeeAlso => {"HodgeIntegrals","hodgeRing","integral"}
     }

document {
     Key => {lambda},
     Headline => "Chern class of the Hodge bundle",
     Usage => "lambda_a",
     Inputs => { "a" => ZZ },
     Outputs => { RingElement },
     "This is an element in the ring created by ",
     TT "hodgeRing",
     ".",
     TEX ///
     It is the $a$-th Chern class of the Hodge bundle on ${\bar M}_{g,n}$.,
     ///,
     PARA {
	  TEX ///
	  Here is a simple example which calculates $\int_{{\bar M}_{1,1}} \lambda_1$.
	  ///,
	  },
     EXAMPLE {
	  "R = hodgeRing (1, 1);",
	  "lambda_1",
	  "integral(1, 1, lambda_1)",
	  },	 
     SeeAlso => {"HodgeIntegrals","hodgeRing","integral"}
     }

document {
     Key => {psi},
     Headline => "cotangent line class",
     Usage => "psi_i",
     Inputs => { "i" => ZZ },
     Outputs => { RingElement },
     "This is an element in the ring created by ",
     TT "hodgeRing",
     ".",
     TEX ///
     It is the $i$-th cotangent line class. Definitions and properties can be found in [AC].
     ///,
     PARA {
	  TEX ///
	  Here is a simple example which calculates $\int_{{\bar M}_{1,1}} \psi_1$.
	  ///,
	  },
     EXAMPLE {
	  "R = hodgeRing (1, 1);",
	  "psi_1",
	  "integral(1, 1, psi_1)",
	  },	 
     SUBSECTION "References",
     PARA {
	  "[AC] ",
	  "Arbarello, E. and Cornalba, M. ",
	  EM "Combinatorial and algebro-geometric cohomology classes on the moduli spaces of curves",
	  ". J. Algebraic Geom. 5. (1996), no. 4, 705--749."
	  },
     SeeAlso => {"HodgeIntegrals","hodgeRing","integral"}
     }

-- Examples from the documentation for hodgeRing
TEST ///
R = hodgeRing (4, 1)
assert(integral (0, 3, 1) == 1)
assert(integral (1, 1, psi_1) == 1/24)
assert(integral (3, 0, lambda_1^6) == 1/90720)
///

-- Examples from the documentation for integral
TEST ///
R = hodgeRing (4, 3);
assert(integral (1, 1, lambda_1) == 1/24)
assert(integral (2, 2, psi_1 * psi_2^2 * lambda_2) == 7/1920)
assert(integral (3, 3, psi_1 * psi_2^2 * psi_3^3 * lambda_3) == 31/16128)
assert(integral (2, 0, lambda_1^3) == 1/2880)
assert(integral (3, 0, lambda_1^6) == 1/90720)
assert(integral (4, 0, lambda_1^9) == 1/113400)
///

-- Examples from the documentation for wittenTau
TEST///
assert(wittenTau (0,{3}) == 1)
assert(wittenTau (0,{4, 1, 1}) == 3)
assert(wittenTau (0,{5, 0, 2}) == 6)
assert(wittenTau (1,{0,1}) == 1/24)
assert(wittenTau (3,{0,0,0,0,0,0,0,1}) == 1/82944)
assert(wittenTau (5,{0,0,0,0,0,3}) == 41873/255467520)
///

-- Examples from the documentation for ch, lambda, kappa, and psi
TEST///
R = hodgeRing (1, 1);
assert(integral(1, 1, ch_1) == 1/24)
assert(integral(1, 1, lambda_1) == 1/24)
assert(integral(1, 1, kappa_1) == 1/24)
assert(integral(1, 1, psi_1) == 1/24)
///

end
--------------------------------------------------------------------------------
restart
uninstallPackage "HodgeIntegrals"
installPackage "HodgeIntegrals"
check "HodgeIntegrals"

loadPackage "HodgeIntegrals";
R = hodgeRing(15,0);
time integral(15,0,kappa_42)
time integral(4,0,lambda_1^9)
time integral(5,0,lambda_1^12)
R = hodgeRing(3,0);
List * List := (A, B) -> apply(A, B, (x, y) -> x * y);
tempFactors = (FactorList, n) -> (
     if #FactorList === 0 then return {splice{n : 1}} else ( 
	  tempList := tempFactors(drop(FactorList, 1), n);
	  a := first FactorList;
	  newList := new List;
	  for i from 1 to n do (
	       aList = splice{i - 1 : 1, a, n - i : 1};
	       newList = append(newList, apply(tempList, x -> aList * x)));
          return flatten newList));
gnList = {{(1,1), (1,2), (0,3)}, {(1,1), (1,1), (0,4)}, {(0,6)}, {(1,3), (0,3)},
         {(1,3), (0,3)}, {(1,2), (0,4)}, {(1,1), (0,5)}};
klpList = {{kappa_3}, {kappa_1, kappa_2}, {kappa_1, kappa_1, kappa_1},
     {kappa_2, lambda_1}, {kappa_1, kappa_1, lambda_1}};
M = matrix table(klpList, gnList, (x,y) -> (sum(tempFactors(x,#y),
	  z-> product(#y, i -> integral(y#i#0, y#i#1, z#i)))));
kernel M     
