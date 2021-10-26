-- -*- coding: utf-8 -*-
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- MULTIPLIER IDEALS -----------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Copyright 2011, 2012, 2013 Claudiu Raicu, Bart Snapp, Zach Teitler
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------

newPackage(
  "MultiplierIdeals",
  Version => "1.1", 
  Date => "April 14, 2015",
  Authors => {
    {
      Name => "Zach Teitler",
      Email => "zteitler@member.ams.org",
      HomePage => "http://math.boisestate.edu/~zteitler/"
    },
    {
      Name => "Bart Snapp"
    },
    {
      Name => "Claudiu Raicu"
    }
  },
  Headline => "multiplier ideals, log canonical thresholds,
    and jumping numbers",
  Keywords => {"D-modules"},
  PackageImports=>{
    "ReesAlgebra",
    "Normaliz"
  },
  PackageExports=>{
    "HyperplaneArrangements"
  },
  DebuggingMode=>false,
  Certification => {
       "journal name" => "The Journal of Software for Algebra and Geometry",
       "journal URI" => "http://j-sag.org/",
       "article title" => "Software for multiplier ideals",
       "acceptance date" => "5 June 2015",
       "published article URI" => "http://msp.org/jsag/2015/7-1/p01.xhtml",
       "published article DOI" => "http://dx.doi.org/10.2140/jsag.2015.7.1",
       "published code URI" => "http://msp.org/jsag/2015/7-1/jsag-v7-n1-x01-MultiplierIdeals.m2",
       "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/MultiplierIdeals.m2",
       "release at publication" => "a71903e3507b0384ece1ed43f815b9344258ed1a",
       "version at publication" => "1.1",
       "volume number" => "7",
       "volume URI" => "http://msp.org/jsag/2015/7-1/"
       }
)

-- Main functionality:
-- Multiplier ideals.
-- Input: an ideal, a real number
-- Output: the multiplier ideal
-- When possible, use specialized routines for
--  monomial ideals (implemented in this package)
--  ideal of monomial curve (implemented in this package)
--  generic determinantal ideals (implemented in this package)
--  hyperplane arrangements (implemented in this package)
-- For arbitrary ideals, load and use the Dmodules package
-- (NOT loaded by this package)


-- Implementation for monomial ideals is based on Howald's Theorem,
--  arXiv:math/0003232v1 [math.AG]
--  J.A. Howald, "Multiplier ideals of monomial ideals.",
--  Trans. Amer. Math. Soc. 353 (2001), no. 7, 2665-2671

-- Implementation for monomial curves is based on the algorithm given in
-- H.M. Thompson's paper: "Multiplier Ideals of Monomial Space
-- Curves." Proc. Amer. Math. Soc. Ser. B 1 (2014), 33–41.

-- Implementation for generic determinantal ideals is based on the
-- dissertation of Amanda Johnson, U. Michigan, 2003

-- Implementation for hyperplane arrangements is based on
-- algorithm given in 
-- Zach Teitler, "A note on Mustață's computation of multiplier ideals
-- of hyperplane arrangements.", Proc. Amer. Math. Soc. 136 (2008),
-- no. 5, 1575--1579 
-- arXiv:math/0610303 [math.AG]
-- The implementation here is a borrowing and modification (with permission)
-- from the HyperplaneArrangements package by Graham Denham & Greg Smith.


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- EXPORTS ---------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

export {
  "multiplierIdeal",
  "logCanonicalThreshold",
  "jumpingNumbers",
  "Interval",
  "IntervalType"
}

-- exportMutable {}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- PACKAGES --------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- Format of command as of Normaliz 2.7:
setNmzOption("bigint",true);


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- METHODS ---------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Here are method functions whose implementations depend heavily on the
-- type of ideal whose multiplier ideal,lct, or jumping numbers are being
-- computed.
-- The methods are grouped by type of ideal: monomial, hyperplane, etc.

-- Exported:
multiplierIdeal = method();
logCanonicalThreshold = method();
jumpingNumbers = method(Dispatch => Thing,
  Options => {
    Interval => {0,infinity},
    IntervalType => "Closed"
  }
);

-- Private:
skodaPeriodicityOnset = method();
jumpingDenominators = method();


--------------------------------------------------------------------------------
-- SHARED ROUTINES -------------------------------------------------------------
--------------------------------------------------------------------------------

intmat2monomIdeal = method();
intmat2monomIdeal ( Matrix, Ring ) := (M,R) -> (
  if ( numColumns M > numgens R ) then (
    error("intmat2monomIdeal: Not enough generators in ring.");
  );
  
  genList := apply( 0..< numRows M ,
                    i -> R_(flatten entries M^{i}) );
  
  return monomialIdeal genList;
);
-- only include rows whose last entry is d; and ignore last column
intmat2monomIdeal ( Matrix, Ring, ZZ ) := (M,R,d) ->
  intmat2monomIdeal(M,R,d,numColumns(M)-1);
-- only include rows with entry 'd' in column 'c'; and ignore column 'c'
intmat2monomIdeal ( Matrix, Ring, ZZ, ZZ ) := (M,R,d,c) -> (
  if ( numColumns M > 1 + numgens R ) then (
    error("intmat2monomIdeal: Not enough generators in ring.");
  );
  
  rowList := select( 0 ..< numRows M , i -> (M_(i,c) == d) ) ;
  columnList := delete( c , toList(0 ..< numColumns M) );
  
  M1 := submatrix(M,rowList,columnList);
  
  return intmat2monomIdeal(M1,R);
);


-- skodaPeriodicityOnset
-- a value sufficiently large that Skoda periodicity has begun:
-- J(I^t) = I.J(I^(t-1)) for t > skodaPeriodicityOnset
-- skodaPeriodicityOnset = min(ambient dimension, numgens I, analyticSpread I)
--   = analyticSpread I
-- for some of the classes of ideals below, we may use a simpler version
-- of the bound than analytic spread
skodaPeriodicityOnset (Ideal) := (I) -> (
  if ( I.cache#?"skodaPeriodicityOnset" ) then (
    I.cache#"skodaPeriodicityOnset"
  ) else (
    I.cache#"skodaPeriodicityOnset" = analyticSpread(I);
    I.cache#"skodaPeriodicityOnset"
  )
)


-- Qinterval
-- give all rational numbers k/denom in the interval [a, b]
-- for one or a list of denominators
Qinterval = method(Options=>{IntervalType=>"Closed"});
Qinterval ( ZZ , Number , Number ) := o -> ( denom, a, b ) -> (
  ql := apply(ceiling(denom*a) .. floor(denom*b) , k -> promote(k/denom,QQ));
  if ( o.IntervalType == "Closed" ) then (
    return ql;
  ) else if ( o.IntervalType == "Open" ) then (
    return select(ql, x -> a<x and x<b);
  ) else if ( o.IntervalType == "ClosedOpen" ) then (
    return select(ql, x -> x<b);
  ) else if ( o.IntervalType == "OpenClosed" ) then (
    return select(ql, x -> a<x);
  );
)
Qinterval ( List , Number , Number ) := o -> ( denoms , a , b ) -> (
  -- empty interval?
  if ( a > b ) then (
    return { };
  );
  qSet := set {};
  for d in denoms do (
    qSet = qSet + set Qinterval( d , a , b , o );
  );
  return sort toList qSet;
)
Qinterval ( ZZ , Number , InfiniteNumber ) := o -> (d,a,b) ->
  Qinterval(d,a,a,o);
Qinterval ( ZZ , InfiniteNumber , Number ) := o -> (d,a,b) -> {};
Qinterval ( ZZ , InfiniteNumber , InfiniteNumber ) := o -> (d,a,b) -> {};
Qinterval ( List , Number , InfiniteNumber ) := o -> (d,a,b) ->
  Qinterval(d,a,a,o);
Qinterval ( List , InfiniteNumber , Number ) := o -> (d,a,b) -> {};
Qinterval ( List , InfiniteNumber , InfiniteNumber ) := o -> (d,a,b) -> {};

Qinterval ( ZZ , List ) := o -> (denom, interval) ->
  Qinterval(denom, interval#0, interval#1, o)
Qinterval ( List , List ) := o -> (denoms, interval) ->
  Qinterval(denoms, interval#0, interval#1, o)



jumpingNumbers Sequence := o -> args -> (
  a := max(o.Interval#0, logCanonicalThreshold(args));
  local b;
  if ( o.Interval#1 == infinity ) then (
    b = skodaPeriodicityOnset(args);
  ) else (
    b = o.Interval#1;
  );
  
  -- potential jumping numbers
  pjn := Qinterval(jumpingDenominators(args),a,b,IntervalType=>o.IntervalType);
  
  if ( #pjn == 0 ) then (
    return { { }, { } }; -- no jumping numbers, no multiplier ideals
  );
  
  local prev;
  local next;
  local jumpingNumbers;
  local multiplierIdeals;
  
  mylct := logCanonicalThreshold(args);
  
  -- Figure out whether pjn#0 is a jumping number:
  -- We know pjn#0 >= mylct.
  -- If pjn#0 == mylct, then it's definitely a jumping number
  -- Otherwise, need to actually check:
  -- we want to compare J(I^p) and J(I^(p-epsilon)) (where p=pjn#0)
  -- We don't know how small epsilon needs to be,
  -- so find the greatest potential jumping number less than p
  -- and use that for p-epsilon
  if ( (pjn#0) == mylct ) then (
    jumpingNumbers = { mylct };
    prev = trim multiplierIdeal append(args,mylct);
    next = prev;
    multiplierIdeals = { prev };
  ) else (
    pjn2 := Qinterval(jumpingDenominators(args), logCanonicalThreshold(args),
      pjn#0 );
    -- pjn2#-1 = pjn#0
    -- The greatest potential jumping number less than p is pjn2#-2
    prev = trim multiplierIdeal append(args,pjn2#-2);
    next = trim multiplierIdeal append(args,pjn#0);
    if ( prev != next ) then (
      -- yes it is a jumping number
      jumpingNumbers = { pjn#0 };
      multiplierIdeals = { next };
    ) else (
      -- no it is not a jumping number
      jumpingNumbers = { };
      multiplierIdeals = { };
    );
  );
  
  -- Now check whether rest of pjn's are jumping numbers:
  
  for i from 1 to (#pjn-1) do (
    
    prev = next;
    next = trim multiplierIdeal append(args,pjn#i);
    
    if ( prev != next ) then (
      jumpingNumbers = append( jumpingNumbers , pjn#i );
      multiplierIdeals = append( multiplierIdeals , next );
    );
  );
  
  return {jumpingNumbers, multiplierIdeals};
)
jumpingNumbers MonomialIdeal := o -> I -> jumpingNumbers(sequence I, o)
jumpingNumbers CentralArrangement := o -> A -> jumpingNumbers(sequence A, o)


--------------------------------------------------------------------------------
-- VIA DMODULES ----------------------------------------------------------------
--------------------------------------------------------------------------------

-*
  Unfortunately it is extremely difficult to achieve
  compatibility with both Dmodules and HyperplaneArrangements,
  due primarily to conflicts in definitions of the method
  lct (defined in both Dmodules and HyperplaneArrangements)
  and the methods multiplierIdeal (Dmodules) and multIdeal
  (HyperplaneArrangements).
  
  At this point we are forced to separate our package from
  either Dmodules or HyperplaneArrangements. We choose to
  remain connected to HyperplaneArrangements for the following
  reasons: Dmodules has a complete treatment of multiplier ideals
  (including jumping numbers); we have little or nothing to add.
  HyperplaneArrangements, on the other hand, is missing a
  treatment of jumping numbers, which our package can supply.
  In addition, the situation of HyperplaneArrangements is in
  keeping with the current emphasis of this package,
  namely, computation of multiplier ideals for various particular
  classes of ideals, using nongeneral routines.
  
  (Note this is the current emphasis, not necessarily the long-term
  goal of this package.)
*-

--------------------------------------------------------------------------------
-- MONOMIAL IDEALS -------------------------------------------------------------
--------------------------------------------------------------------------------

-*
  Code in this section written by Zach Teitler 2010, 2011, 2012
*-

-- NewtonPolyhedron
-- compute a matrix A such that Ax >= 0 defines the cone over
-- the Newton polyhedron of a monomial ideal
-- (ie Newt(I) is placed at height 1)
-- Uses Normaliz
NewtonPolyhedron = method();
NewtonPolyhedron (MonomialIdeal) := I -> (
  
  R := ring I;
  -- use R;
  nmzFilename = temporaryFileName() ;
  setNmzOption("supp",true);
  
  -- Compute equations of supporting hyperplanes of (cone over) Newt(I)
  normaliz( matrix(I / exponents / flatten) , "rees_algebra");
  M := readNmzData("sup");
  
  -- Clean up tmp files, options
  setNmzOption("supp",false);
  rmNmzFiles();
  
  return M;
  
);

-- multiplierIdeal of monomialIdeal
-- input: monomialIdeal I, rational number t
-- output: multiplier ideal J(I^t)
----
---- How it works:
----
---- First, compute integer matrix M defining Newton polyhedron Newt(I)
---- in the sense that M = (A|-b) where x^v \in I iff M(v|1) >= 0,
---- ie, Av >= b.
----   Some rows are ``unit rows'' with a single nonzero entry which is a 1,
---- corresponding to requiring the exponents (entries of v) to be >= 0.
---- These define *coordinate* facets of Newt(I) (ie, facets of Newt(I)
---- contained in facets of the positive orthant). All other rows define
---- *non-coordinate* facets.
----
---- Second, get an integer matrix for t*Newt(I) by writing t=p/q
---- and setting M1 = (pA | -qb).
----
---- The inequality
----    (pA) * v >= (qb),
---- or
----    M1 * ( v | 1 ) >= 0,
---- corresponds to t*Newt(I). From Howald's Theorem, we need the interior,
---- Int(t*Newt(I)). It is not quite correct to take M1 * (v | 1) > 0,
---- because this is wrong for the coordinate faces. (It is correct for the
---- non-coordinate faces.) Here "interior" means: relative to the positive
---- orthant. In other words, we are removing the part of the boundary of
---- t*Newt(I) which is in the interior of the positive orthant.
----
---- Let 'bump' be a vector of the same length as b, with entries 0 or 1:
----   the entry is 0 in each row of M1 corresponding to a coordinate face,
----   the entry is 1 in each row of M1 corresponding to a non-coordinate face.
---- Then the lattice points in Int(t*Newt(I)) satisfy
----    (pA | -qb-bump) * (v | 1 ) >= 0,
---- that is
----    (pA)*v >= qb+bump.
---- For integer points with nonnegative entries this is equivalent to
----    (pA)*v > qb.
----
---- Finally, we use Normaliz to compute the monomial ideal containing x^v
---- for v in Int(t*Newt(I)); then use Macaulay2 to quotient by the product
---- of the variables, corresponding to Howald's (1,...,1).

multiplierIdeal (MonomialIdeal, ZZ) := (I,t) -> multiplierIdeal(I,promote(t,QQ))
multiplierIdeal (MonomialIdeal, QQ) := (I,t) -> (
  
  R := ring I;
  -- use R;
  local multIdeal;
  
  
  if ( t <= 0 ) then (
    
    multIdeal = monomialIdeal(1_R) ;
  
  ) else if ( I == ideal(0_R) ) then (
    
    multIdeal = monomialIdeal(0_R);
  
  ) else if ( I == ideal(1_R) ) then (
  
    multIdeal = monomialIdeal(1_R);
  
  ) else if ( t >= skodaPeriodicityOnset I ) then (
    
    s := 1 + floor(t-skodaPeriodicityOnset(I));
    multIdeal = I^s*multiplierIdeal(I,t-s) ;
  
  ) else (
    
    M := NewtonPolyhedron(I);
    m := numRows M;
    n := numColumns M;
    -- ambient dimension = n-1 !!
    
    nmzFilename = temporaryFileName() ;
    setNmzOption("normal",true);
    
    -- Scale to get t*Newt(I) (clear denominators)
    p := numerator t;
    q := denominator t;
    M1 := M * diagonalMatrix( flatten { toList((n-1):q) , {p} } );
    
    -- Set up "bump" of nontrivial facets, but don't bump coordinate facets
    -- (except we do end up bumping the row (0,..,0,1); but that's okay,
    -- no harm)
    bump := apply(toList(0..<m) ,
      i -> (  if ( M_(i,n-1) >= 0 ) then ( return 0; ) else ( return 1; )  ) );
    -- now bump has a 0 in rows corresponding to coordinate facets, 1 in other
    -- rows
  
    -- "Bump" t*Newt(I): push nontrivial facets in by "epsilon" (which is 1)
    M2 := M1 - ( matrix(toList(m:toList((n-1):0))) | transpose matrix({bump}) );
    
    -- Semigroup of lattice points inside "bumped" region (i.e., interior of
    -- t*Newt(I))
    nmzOut := normaliz(M2,"inequalities",grading=>toList(numColumns(M2):1));
    M3 := nmzOut#"gen";
    
    -- Ideal generated by those lattice points
    J := intmat2monomIdeal(M3,R,1);
    
    -- Howald's translation by (1,1,...,1) is same as colon with product of
    -- variables
    multIdeal = monomialIdeal quotient(J, product(flatten entries vars R)) ;
    
    -- Clean up tmp files, options
    setNmzOption("normal",false);
    rmNmzFiles();
    
  );
  
  return trim multIdeal;

);


-- logCanonicalThreshold ( MonomialIdeal , RingElement )
-- threshold of inclusion in a multiplier ideal
-- input:
--  1. a monomial ideal I
--  2. a monomial x^v, or exponent vector v
-- output: a pair
--  1. a rational number t, defined by
--        t = sup { c : x^v is in the c'th multiplier ideal of I }
--     the threshold (of inclusion of x^v in the multiplier ideal of I).
--  2. a matrix (A' | -b') consisting of those rows of the defining matrix of
--     Newt(I) which impose the threshold on x^v.
--  In other words, the line joining the origin to the vector (v+(1,..,1)) hits
--  the boundary of Newt(I) at (1/t)*(v+(1,..,1)), and the vector
--  (1/t)*(v+(1,..,1)) lies on the facets defined by the rows of (A' | -b')
--  (via: (A'|-b')(w|1)^transpose >= 0 )
logCanonicalThreshold (MonomialIdeal , RingElement) := (I , mon) -> (
  if ( leadMonomial(mon) != mon ) then (
    error("Second input must be a monomial (input was ",mon,")");
  );
  
  R := ring I;
  

  emptyMatrix := (matrix {{}})^{}; -- 0-by-0 matrix
  if ( I == ideal(0_R) ) then (
    return ( 0 , emptyMatrix );
  ) else if ( I == ideal(1_R) ) then (
    return ( infinity , emptyMatrix );
  );
  
  v := first exponents mon;
  M := NewtonPolyhedron(I);
  m := numRows M;
  n := numColumns M;
  
  local i;
  threshVal := infinity;
  facetList := {}; -- list of rows
  
  for i from 0 to m-1 do (
    s := sum( toList(0..(n-2)) , j -> M_(i,j)*(1+v_j) );
    if ( M_(i,n-1) != 0 and s != 0 ) then (
      t := -1*s / M_(i,n-1) ;
      if ( t < threshVal ) then (
        threshVal = t;
        facetList = {i};
      ) else if ( t == threshVal ) then (
        facetList = append(facetList , i);
      );
    );
  );
  
  facetMatrix := M^facetList;
  
  return ( threshVal , facetMatrix );
);

logCanonicalThreshold (MonomialIdeal) := I ->
  first logCanonicalThreshold ( I , 1_(ring(I)) )

skodaPeriodicityOnset (MonomialIdeal) := I -> (
  if I.cache#?"skodaPeriodicityOnset" then (
    I.cache#"skodaPeriodicityOnset"
  ) else (
    R := ring I;
    if ( I == monomialIdeal(0_R) or I == monomialIdeal(1_R) ) then (
      I.cache#"skodaPeriodicityOnset" = 0;
    ) else (
      I.cache#"skodaPeriodicityOnset" = analyticSpread I;
    );
    
    I.cache#"skodaPeriodicityOnset"
  )
)

jumpingDenominators (MonomialIdeal) := I -> (
  
  R := ring I;
  if ( I == monomialIdeal(0_R) or I == monomialIdeal(1_R) ) then (
    return {1};
  );
  
  denomList := {};
  M := NewtonPolyhedron(I);
  m := numRows M;
  n := numColumns M;
  
  for i from 0 to m-1 do (
    s := sum drop(flatten entries M^{i}, -1);
    if ( M_(i,n-1) != 0 and s != 0 ) then (
      denomList = append(denomList , -M_(i,n-1));
    );
  );
  return denomList;
)


--------------------------------------------------------------------------------
-- MONOMIAL CURVES -------------------------------------------------------------
--------------------------------------------------------------------------------

-*
  Code in this section written by Claudiu Raicu, Bart Snapp,
  Zach Teitler 2011, 2012
*-

-- affineMonomialCurveIdeal
--
-- Compute defining ideal of a curve in affine 3-space parametrized by
-- monomials, i.e., parametrized by t -> (t^a,t^b,t^c) for positive integers
-- a,b,c.
--
-- Input:
--  * ring S
--  * list of integers {a,b,c}
-- Output:
--  * ideal (ideal in S defining curve parametrized by t->(t^a,t^b,t^c))
--
-- The ring S should be a polynomial ring over a field. Currently this
-- is not checked.  The integers {a,b,c} should be positive. Currently
-- this is not checked.  The output ideal may need to be trimmed, we
-- do not do this.
--
-- The code for affineMonomialCurveIdeal is based on the code for
-- monomialCurveideal from Macaulay2.

affineMonomialCurveIdeal = (S, a) -> (
  -- check that S is a polynomial ring over a field
  n := # a;
  if not all(a, i -> instance(i,ZZ) and i >= 1)
  then error "expected positive integers";
  t := symbol t;
  k := coefficientRing S;
  M1 := monoid [t];
  M2 := monoid [Variables=>n];
  R1 := k M1;
  R2 := k M2;
  t = R1_0;
  mm := matrix table(1, n, (j,i) -> t^(a#i));
  j := generators kernel map(R1, R2, mm);
  ideal substitute(j, submatrix(vars S, {0..n-1}))
)


-- ord
--
-- Compute monomial valuation of a given polynomial with respect to a
-- vector that gives the values of the variables.
--
-- Input:
--  * list mm = {a1,a2,a3,...}
--  * polynomial p
-- Output:
--  * integer
--
-- This computes the monomial valuation in which the variable x1 has
-- value a1, x2 has value a2,...  The value of a polynomial is the
-- MINIMUM of the values of its terms (like order of vanishing, NOT
-- like degree).
--
-- The values {a1,a2,...} should be nonnegative and there should be at
-- least as many as the number of variables appearing in the
-- polynomial. Currently we do not check this.

ord = (mm,p) -> (
  R := ring p;
  degs := apply(listForm p, i-> first i);
  min apply(degs, i -> sum apply(i,mm,times))
)


-- sortedGens
--
-- Compute the minimal generators of the defining ideal of the
-- monomial curve parametrized by t->(t^a1,t^a2,t^a3,...) and return
-- the list of generators in order of increasing values of
-- ord({a1,a2,a3,...}, -).
--
-- Input:
--  * ring R
--  * list nn = {a1,a2,a3,...} of integers
-- Output:
--  * list of polynomials
--
-- The ring R should be a polynomial ring over a field. Currently this
-- is not checked.  The integers {a1,a2,a3,...} should be
-- positive. Currently this is not checked.

sortedGens = (R,nn) -> (
  KK := coefficientRing R;
  genList := flatten entries gens trim affineMonomialCurveIdeal(R,nn);
  L := sort apply(genList, i -> {ord(nn,i), i});
  apply(L, i-> last i)     
)


-- exceptionalDivisorValuation
--
-- Compute the valuation induced by the (mm,ord(mm,f_2)) exceptional
-- divisor in the resolution of singularities of the monomial curve
-- with exponent vector nn.
--
-- Input:
--  * list of integers nn={a,b,c}
--  * list of integers mm={d,e,f}
--  * polynomial p (in 3 variables)
-- Output:
--  * integer
--
-- The valuation is defined as follows. First we computed the sorted
-- generators (f0,f1,f2,...)  of the defining ideal of the
-- curve. Writing p = f0^d * g where g is not divisible by f0, the
-- valuation of p is d*ord(mm,f1) + ord(mm,g).

exceptionalDivisorValuation = (nn,mm,p) -> (
  R := ring p;
  ff := sortedGens(R,nn);
  n := 0;
  while p % ff_0 == 0 do (p = p//ff_0; n = n+1;);
  n*ord(mm,ff_1) + ord(mm,p)
)


-- exceptionalDivisorDiscrepancy
--
-- Compute the multiplicity of the relative canonical divisor along
-- the (mm,ord(mm,f_2)-ord(mm,f_1)) exceptional divisor in the
-- resolution of singularities of a monomial curve.
--
-- Input:
--  * list of integers mm={a,b,c}
--  * sorted list of generators of the ideal of the monomial curve
-- Output:
--  * integer

exceptionalDivisorDiscrepancy = (mm,ff) -> (
  sum(mm) - 1 + ord(mm, ff_1) - ord(mm, ff_0)
)

-- monomialValuationIdeal
--
-- Compute valuation ideal {h : ord(mm,h) >= val}.
--
-- Input:
--  * ring R
--  * list of integers mm={a1,a2,...}
--  * integer val
-- Output:
--  * ideal of R.
-- The ring R should be a polynomial ring over a field.
-- The list mm should have nonnegative integers, with at least as many as the
-- number of variables in R. Currently we do not check these things.

monomialValuationIdeal = (R,mm,val) -> (
  M := (matrix{mm}|matrix{{-val}}) || id_(ZZ^(#mm+1));
  setNmzOption("normal",true);
  normalizOutput := normaliz(M,"inequalities",grading=>toList(numColumns(M):1));
  M2 := normalizOutput#"gen";
  setNmzOption("normal",false);
  intmat2monomIdeal(M2,R,1)
)


-- exceptionalDivisorValuationIdeal
--
-- Compute valuation ideal {h : v(h) >= val}, where the valuation v is induced
-- by the (mm,ord(mm,f_2)-ord(mm,f_1)) exceptional divisor.
--
-- Input:
--  * ring R
--  * sorted list of generators of curve ideal
--  * list mm={a,b,c}
--  * integer val
-- Output:
--  * ideal

exceptionalDivisorValuationIdeal = (R,ff,mm,val) -> (
  maxpow := ceiling(val / ord(mm,ff_1));
  if maxpow < 0 then ideal(1_R) else
  sum apply(splice{0..maxpow},
    i -> ideal(ff_0^i)*monomialValuationIdeal(R,mm,val-i*ord(mm,ff_1)))
)


-- termIdeal
--
-- Compute smallest monomial ideal containing a given ideal.
--
-- Input:
--  * ideal
-- Output:
--  * monomialIdeal

termIdeal = I -> (
  R := ring I;
  if I == ideal 0_R then return monomialIdeal 0_R else
  return monomialIdeal flatten apply(flatten entries gens I, i -> terms i)
)

-- symbolicPowerCurveIdeal
--
-- Compute symbolic power of the defining ideal of a monomial space curve.
--
-- Input:
--  * ideal I
--  * integer t
-- Output:
--  * ideal
--
-- For a prime ideal I and p>=0, the symbolic power I^(p) is the ideal of
-- functions vanishing to order at least p at every point of V(I).
-- It is the I-primary component of I^p. The non-I-primary
-- components of I^p have support contained in Sing(V(I)).
--
-- For our ideals (of monomial curves) the singular locus is a single point,
-- the origin. We compute the symbolic power by computing I^p, then saturating
-- with respect to the ideal of the origin (to remove unwanted primary
-- components).
--
-- In the future this may be replaced by a better algorithm, perhaps?
--
-- We assume the input ideal is indeed prime, and that its unique singular point
-- is the origin.

symbolicPowerCurveIdeal = (I,t) -> saturate(I^(max(0,t)))


-- intersectionIndexSet
--
-- Compute indexing set for intersection appearing in the formula for multiplier
-- ideals. This is a finite set of lattice points defined by some equations and
-- inequalities. See H.M. Thompson's paper (cited above).
--
-- Input:
--  * ring
--  * list of exponents
-- Output:
--  * list (of lattice points, each written as a list of integers)
--

-- Now, I'm commenting out a big block of code that was written by
-- Claudiu, Bart, and me in 2011, more or less directly translating
-- a preprint of H.M. Thompson.
-- First of all the code below has a bug (can't handle curves coming from
-- exponents {a,b,c} where c >= a*b ! didn't notice that until now...).
-- I know I should figure out what went wrong, but...
-- Second of all, H.M. Thompson has told me that in fact the situation
-- is much easier, at least for these monomial space curves anyway:
-- the intersection index set is simply the exponent vector
-- (the index set is a singleton)!
-- So, for now anyway, just return that.
-- Eventually we'll want to generalize beyond monomial curves,
-- at which point this will all get straightened out.
--
-- intersectionIndexSet = (ff) -> (
--   uu := {(exponents(ff_0))_0, (exponents(ff_1))_0};
--   vv := {(exponents(ff_0))_1, (exponents(ff_1))_1};
--   
--   cols := #(uu_0);
--   candidateGens1 := (normaliz(matrix{uu_0 - vv_0} || matrix{vv_0 - uu_0} ||
--     matrix{uu_1 - vv_1} || id_(ZZ^cols),4))#"gen";
--   candidateGens2 := (normaliz(matrix{uu_0 - vv_0} || matrix{vv_0 - uu_0} ||
--     matrix{vv_1 - uu_1} || id_(ZZ^cols),4))#"gen";
--   candidateGens  := candidateGens1 || candidateGens2;
--   rhoEquation    := (transpose matrix {uu_1-uu_0}) |
--     (transpose matrix {vv_1-vv_0});
--   
--   T := candidateGens * rhoEquation;
--   rows := toList select(0..<numRows T, i ->
--     all(0..<numColumns T, j -> T_(i,j) > 0));
--   unique apply(rows, i -> flatten entries candidateGens^{i})
-- )
intersectionIndexSet = (R,exps) -> {exps}


-- multiplierIdeal of MonomialCurve
--
-- Compute multiplier ideal of the defining ideal of a monomial space curve,
-- ie., a curve in affine 3-space parametrized by monomials, t->(t^a,t^b,t^c).
--
-- Input:
--  * ring R
--  * list of integers {a,b,c}, the exponents in the parametrization
--  * an integer or rational number t
-- Output:
--  * an ideal

multiplierIdeal (Ring, List, ZZ) := (R, nn, t) ->
  multiplierIdeal(R,nn,promote(t,QQ))
multiplierIdeal (Ring, List, QQ) := (R, nn, t) -> (
  ff := sortedGens(R,nn);
  curveIdeal := affineMonomialCurveIdeal(R,nn);
  
  indexList := intersectionIndexSet(R,nn);
  
  
  symbpow := symbolicPowerCurveIdeal(curveIdeal , floor(t-1));
  term    := multiplierIdeal(termIdeal(curveIdeal) , t);
  
  validl  := intersect apply(indexList ,
    mm -> exceptionalDivisorValuationIdeal(R,ff,mm,
      floor(t*ord(mm,ff_1)-exceptionalDivisorDiscrepancy(mm,ff)) ));
  
  trim intersect(symbpow,term,validl)
)




-- logCanonicalThreshold of MonomialCurve
--
-- Compute log canonical threshold of the defining ideal of a monomial
-- space curve, ie., a curve in affine 3-space parametrized by
-- monomials, t->(t^a,t^b,t^c).
--
-- Input:
--  * ring R
--  * list of integers {a,b,c}, the exponents in the parametrization
-- Output:
--  * a rational number

logCanonicalThreshold(Ring,List) := (R,nn) -> (
  ff := sortedGens(R,nn);
  indexList  := intersectionIndexSet(R,nn);
  curveIdeal := ideal ff;
  lctTerm    := logCanonicalThreshold(termIdeal(curveIdeal));
  min (2, lctTerm, 
    min(
         apply(indexList, mm ->
           (exceptionalDivisorDiscrepancy(mm,ff)+1)/ord(mm,ff_1) )
    ) )
)

skodaPeriodicityOnset (Ring,List) := (R,exps) ->
  analyticSpread affineMonomialCurveIdeal(R,exps)

jumpingDenominators (Ring,List) := (R,exps) -> (
  ff := sortedGens(R,exps);
  indexList  := intersectionIndexSet(R,exps);
  curveIdeal := ideal ff;
  sort unique({1} | apply(indexList, m -> ord(m,ff_1)) |
    jumpingDenominators(termIdeal(curveIdeal)))
)


--------------------------------------------------------------------------------
-- HYPERPLANE ARRANGEMENTS -----------------------------------------------------
--------------------------------------------------------------------------------

-- Based on code written by Graham Denham and Greg Smith for
-- their HyperplaneArrangements package

simpleCacheKey := getSymbol "simple";
multiplicitiesCacheKey := getSymbol "m";
irredCacheKey := getSymbol "irreds";

-- rank (Flat) := ZZ => F -> rank subArrangement F
weight := (F,m) -> sum((tolist F)/(i->m_i))
normal := h -> (
     h/leadCoefficient h);  -- representative of functional, mod scalars

-- trim defined in HyperplaneArrangements has this problem:
-- its results are put into the cache, in *unusable* keys :-(
-- A.cache.simple, A.cache.m -- but "simple" and "m" are protected,
-- unexported symbols.
-- As soon as they fix their package then this "betterTrim"
-- should be deleted from here, and we just use their trim
betterTrim := A -> (
  if A.cache#?simpleCacheKey then return(A.cache#simpleCacheKey);
  if (tolist A == {}) then (
    A.cache#simpleCacheKey = A;
    A.cache#multiplicitiesCacheKey = {};
    return A;
  ) else (
    count := new MutableHashTable;
    scan(tolist A, h -> (
      if h != 0 then (
        if not count#?(normal h) then count#(normal h) = 0;
        count#(normal h) = 1+count#(normal h);
      )
    ));
    (L,m) := (keys(count),values(count));
    A' := arrangement(L, ring A);
    A.cache#multiplicitiesCacheKey = m; 
    A.cache#simpleCacheKey = A';
    return A';
  );
);

irreducibles := A -> (
  if not A.cache#?irredCacheKey then
    A.cache#irredCacheKey = select(flatten drop(flats(A),1), F->(0 != euler F));
  return A.cache#irredCacheKey;
);

multiplierIdeal (CentralArrangement,List,Number) := Ideal => (A,m,s) -> (
  if (#tolist A != #m) then error "expected one weight for each hyperplane";
  R := ring A;
  
  if ( betterTrim(A) == arrangement({},R)
    or betterTrim(A) == arrangement({0_R},R) ) then (
    if ( s <= 0 ) then (
      return ideal(1_R);
    ) else (
      return ideal(0_R);
    );
  ) else if ( betterTrim(A) == arrangement({1_R},R) ) then (
    return ideal(1_R);
  );
  
  irreds := irreducibles(A);
  exps := irreds/(F->max(0,floor(s*weight(F,m))-rank(F)+1));
  ideals := irreds/(F-> trim ideal tolist (A_F));
  return intersect apply(#exps, i->(ideals_i)^(exps_i));
);
multiplierIdeal (CentralArrangement,Number) := Ideal => (A,s) -> (
  betterTrim A;
  return multiplierIdeal(A.cache#simpleCacheKey,
    A.cache#multiplicitiesCacheKey,s);
);

logCanonicalThreshold(CentralArrangement,List) := (A,m) -> (
  if (#tolist A != #m) then error "expected one weight for each hyperplane";
  R := ring A;
  if ( betterTrim(A) == arrangement({},R)
    or betterTrim(A) == arrangement({0_R},R) ) then (
    return 0;
  ) else if ( betterTrim(A) == arrangement({1_R},R) ) then (
    return infinity;
  );
  irreds := irreducibles(A);
  return min(irreds/(F -> (rank(F)/weight(F,m))));
);
logCanonicalThreshold(CentralArrangement) := A -> (
  betterTrim A;
  return logCanonicalThreshold(A.cache#simpleCacheKey,
    A.cache#multiplicitiesCacheKey);
);

skodaPeriodicityOnset(CentralArrangement) := A -> 1
-- Assuming all multiplicities non-negative integers:
skodaPeriodicityOnset(CentralArrangement,List) := (A,m) -> 1

jumpingDenominators(CentralArrangement,List) := (A,m) -> (
  if (#tolist A != #m) then error "expected one weight for each hyperplane";
  R := ring A;
  if ( betterTrim(A) == arrangement({},R)
    or betterTrim(A) == arrangement({0_R},R)
    or betterTrim(A) == arrangement({1_R},R) ) then (
      return {1};
  );
  irreds := irreducibles(A);
  return unique sort (irreds/(F -> weight(F,m)));
);
jumpingDenominators(CentralArrangement) := A -> (
  betterTrim A;
  return jumpingDenominators(A.cache#simpleCacheKey,
    A.cache#multiplicitiesCacheKey);
);




--------------------------------------------------------------------------------
-- GENERIC DETERMINANTS --------------------------------------------------------
--------------------------------------------------------------------------------

genericDeterminantalSymbolicPower = method();
genericDeterminantalSymbolicPower (Matrix,ZZ,ZZ) := (M,r,a) -> (
  R := ring M;
  I := ideal(0_R);
  local J;
  for p in partitions(a) do (
    J = ideal(1_R);
    for i from 0 to (#p - 1) do (
      J = trim (J * minors(r - 1 + p#i, M));
    );
    
    I = I + J;
  );
  
  return trim I;
)


-- multiplierIdeal (Ring,List,ZZ,ZZ) := (R,mm,r,c) ->
--   multiplierIdeal(R,mm,r,promote(c,QQ))
-- multiplierIdeal (Ring,List,ZZ,QQ) := (R,mm,r,c) -> (
--   m := mm_0;
--   n := mm_1;
--   if ( m*n > numcols vars R ) then (
--     error "not enough variables in ring";
--   );
--   X := genericMatrix(R,m,n);
--   
--   multiplierIdeal(X,r,c)
-- )
multiplierIdeal (Matrix,ZZ,ZZ) := (X,r,c) -> multiplierIdeal(X,r,promote(c,QQ))
multiplierIdeal (Matrix,ZZ,QQ) := (X,r,c) -> (
  R := ring(X);
  m := numRows(X);
  n := numColumns(X);
  
  -- if minors larger than size of matrix, then "minors" generate zero ideal
  if ( r > min(m,n) ) then (
    if ( c <= 0 ) then (
      return ideal(1_R);
    ) else (
      return ideal(0_R);
    );
  );
  
  -- should do:
  -- use skodaPeriodicityOnset to reduce coefficient 'c' before computing this
  
  J := ideal(1_R);
  for i from 1 to r do (
    ai := max(0, floor( c * (r+1-i) ) + 1 - (n-i+1)*(m-i+1));
    Ji := genericDeterminantalSymbolicPower(X,i,ai);
    J = intersect(J,Ji);
  );
  
  return J;
)

-- logCanonicalThreshold(List,ZZ) := (mm,r) -> min( apply(0..<r ,
--   i -> (mm_0-i)*(mm_1-i)/(r-i)) )
-- logCanonicalThreshold(Ring,List,ZZ) := (R,mm,r) ->
--   logCanonicalThreshold(mm,r)
logCanonicalThreshold(Matrix,ZZ) := (M,r) -> (
  if ( r <= 0 ) then (
    return infinity;
  ) else if ( r > min(numRows(M),numColumns(M)) ) then (
    return 0;
  ) else (
    return min( apply(0..<r , i -> (numRows(M)-i)*(numColumns(M)-i)/(r-i)) );
  );
)

-- skodaPeriodicityOnset(List,ZZ) := (mm,r) -> (
--   ambdim := mm_0 * mm_1;
--   idealgens := binomial(mm_0,r) * binomial(mm_1,r);
--   return min(ambdim, idealgens);
-- )
-- skodaPeriodicityOnset(Ring,List,ZZ) := (R,mm,r) ->
--   skodaPeriodicityOnset(mm,r)
skodaPeriodicityOnset(Matrix,ZZ) := (M,r) -> (
  ambdim := numRows(M) * numColumns(M);
  idealgens := binomial(numRows M,r) * binomial(numColumns M,r);
  return min(ambdim, idealgens);
)

-- jumpingDenominators(Ring,List,ZZ) := (R,mm,r) -> toList(1..r-1)
-- jumpingDenominators(List,ZZ) := (mm,r) -> toList(1..r-1)
jumpingDenominators(Matrix,ZZ) := (M,r) -> (
  if ( r <= 0 or r > min(numRows(M),numColumns(M)) ) then (
    return {1};
  ) else (
    return toList(1..r-1);
  );
)


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- TESTS -----------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- SHARED ROUTINES -------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- VIA DMODULES ----------------------------------------------------------------
--------------------------------------------------------------------------------

-- TEST ///
--   needsPackage "MultiplierIdeals";
--   R = QQ[x,y];
--   -- use R;
--   I = ideal(y^2-x^3,R);
--   assert(logCanonicalThreshold(I) == 5/6);
--   assert(multiplierIdealViaDmodules(I,1/2) == ideal(1_R));
--   assert(multiplierIdealViaDmodules(I,5/6) == ideal(x,y));
--   assert(multiplierIdealViaDmodules(I,1) == I);
-- ///  

--------------------------------------------------------------------------------
-- MONOMIAL IDEALS -------------------------------------------------------------
--------------------------------------------------------------------------------

-- Compute a NewtonPolyhedron and intmat2monomialIdeal:
-- go from Ideal -> Polyhedron -> Ideal, see if it is the same again
TEST ///
  needsPackage "Normaliz";
  needsPackage "MultiplierIdeals";
  debug MultiplierIdeals;
  R = QQ[x,y,z];
  -- use R;
  I = monomialIdeal(x*z^2,y^3,y*z^3,y^2*z^2,x*y^2*z,x^2*y*z,x^3*z,x^2*y^2,
    x^4*y,x^5,z^6);
  -- this I is integrally closed!
  M1 = NewtonPolyhedron(I); -- integer matrix (A|b) s.t. Newt(I) = {Ax \geq b}
  setNmzOption("normal",true);
  nmzOut = normaliz(M1,"inequalities",grading=>toList(numColumns(M1):1));
  M2 = nmzOut#"gen"; -- integer matrix whose rows (minimally) generate semigroup 
   -- of lattice points in {Ax \geq b}, where M1 = (A|b)
  setNmzOption("normal",false);
  J = intmat2monomIdeal(M2,R,1); -- integer matrix -> ideal
  assert ( I === J );
///



-- Compute some LCTs of diagonal monomial ideals
TEST ///
  needsPackage "MultiplierIdeals";
  R = QQ[x_1..x_5];
  -- use R;
  for a from 2 to 6 do (
    for b from a to 6 do (
      for c from b to 6 do (
        for d from c to 6 do (
          for e from d to 6 do (
            I := monomialIdeal(x_1^a,x_2^b,x_3^c,x_4^d,x_5^e);
            l := 1/a+1/b+1/c+1/d+1/e;
            assert ( logCanonicalThreshold I === l );
          );
        );
      );
    );
  );
///

TEST ///
  R = QQ[x,y];
  I = monomialIdeal(y^2,x^3);
  JN = jumpingNumbers(I);
  assert ( JN#0 === {5/6, 7/6, 4/3, 3/2, 5/3, 11/6, 2/1} );
  assert ( JN#1 == {ideal (x,y),
                    ideal (x^2,y),
                    ideal (x^2,x*y,y^2),
                    ideal (x^3,x*y,y^2),
                    ideal (x^3,x^2*y,y^2),
                    ideal (x^4,x^2*y,x*y^2,y^3),
                    ideal (x^4,x^3*y,x*y^2,y^3)} );
///

TEST ///
  R = QQ[x,y,z];
  I = monomialIdeal( x*y , x*z , y*z );
  JN1 = jumpingNumbers(I);
  assert ( JN1#0 === {3/2, 2/1, 5/2, 3/1} );
  assert ( JN1#1 == {ideal (x,y,z),
                     ideal (x*y,x*z,y*z),
                     ideal (x^2*y,x*y^2,x^2*z,x*y*z,y^2*z,x*z^2,y*z^2),
                     ideal (x^2*y^2,x^2*y*z,x*y^2*z,x^2*z^2,x*y*z^2,y^2*z^2)} );
  
  JN2 := jumpingNumbers(I^2);
  assert ( JN2#0 === {3/4, 1/1, 5/4, 3/2, 7/4, 2/1, 9/4, 5/2, 11/4, 3/1} );
  
  II = I^2 + monomialIdeal(x*y*z);
  assert ( II === monomialIdeal( x^2*y^2 , x^2*z^2 , y^2*z^2 , x*y*z ) ) ;
  JN3 = jumpingNumbers(II);
  assert ( JN3#0 === {1/1, 3/2, 2/1} );
  assert ( JN3#1 == {ideal (x*y,x*z,y*z),
                     ideal (x^2*y^2,x*y*z,x^2*z^2,y^2*z^2),
                     ideal (x^3*y^3,x^2*y^2*z,x^2*y*z^2,x*y^2*z^2,x^3*z^3,
                       y^3*z^3)} );
///

-- Threshold computations
TEST ///
  needsPackage "MultiplierIdeals";
  R := QQ[x,y];
  -- use R;
  I := monomialIdeal( y^2 , x^3 );
  assert ( logCanonicalThreshold( I , 1_R )
    === (5/6,map(ZZ^1,ZZ^3,{{2, 3, -6}})) );
  assert ( logCanonicalThreshold( I , x )
    === (7/6,map(ZZ^1,ZZ^3,{{2, 3, -6}})) );
  I = monomialIdeal( x^3 , x*y , y^4 );
  assert ( logCanonicalThreshold( I , 1_R )
    === (1/1,map(ZZ^2,ZZ^3,{{1, 2, -3}, {3, 1, -4}})) );
  assert ( logCanonicalThreshold( I , x )
    === (4/3,map(ZZ^1,ZZ^3,{{1, 2, -3}})) );
///

TEST ///
  R = QQ[x,y,z];
  I = monomialIdeal(x^8,y^6); -- Example 2 of [Howald 2000]
  assert ( multiplierIdeal(I,1)
    == monomialIdeal (x^6,x^5*y,x^4*y^2,x^2*y^3,x*y^4,y^5) );
  I = monomialIdeal(x*y^4*z^6, x^5*y, y^7*z, x^8*z^8);
    -- Example 7 of [Howald 2000]
  assert ( logCanonicalThreshold(I) == 68/191 );
///

-- Zero ideal and unit ideal
TEST ///
  needsPackage "MultiplierIdeals";
  R = QQ[x];
  I = monomialIdeal(0_R);
  assert ( multiplierIdeal(I,1) == monomialIdeal(0_R) );
  assert ( multiplierIdeal(I,3/2) == monomialIdeal(0_R) );
  assert ( multiplierIdeal(I,0) == monomialIdeal(1_R) );
  assert ( logCanonicalThreshold(I) == 0 );
  assert ( first logCanonicalThreshold(I,x) == 0 );
///

TEST ///
  needsPackage "MultiplierIdeals";
  R = QQ[x];
  I = monomialIdeal(1_R);
  assert ( multiplierIdeal(I,1) == monomialIdeal(1_R) );
  assert ( multiplierIdeal(I,3/2) == monomialIdeal(1_R) );
  assert ( multiplierIdeal(I,0) == monomialIdeal(1_R) );
  assert ( logCanonicalThreshold(I) == infinity );
  assert ( first logCanonicalThreshold(I,x) == infinity );
///

--------------------------------------------------------------------------------
-- MONOMIAL CURVES -------------------------------------------------------------
--------------------------------------------------------------------------------

TEST ///
needsPackage"MultiplierIdeals";
debug MultiplierIdeals;
R = QQ[x,y,z];
assert( (affineMonomialCurveIdeal(R,{2,3,4})) == ideal(y^2-x*z,x^2-z) )
assert( (affineMonomialCurveIdeal(R,{5,8})) == ideal(x^8-y^5) )
assert( (affineMonomialCurveIdeal(R,{1,1,1})) == ideal(y-z,x-z) )
///

TEST ///
needsPackage"MultiplierIdeals";
debug MultiplierIdeals;
R = QQ[x,y,z];
assert( (ord({2,3,4},z-x^2)) === 4 )
assert( (ord({1,1,1},z*y+x-x^2)) === 1 )
assert( (ord({0,0,0},(z*y+x-x^2)^2)) === 0 )
assert( (ord({2,3,4},1+x)) === 0 )
///

TEST ///
needsPackage"MultiplierIdeals";
debug MultiplierIdeals;
R = QQ[x,y,z];
assert( {x^2-z,y^2-x*z} === (sortedGens(R,{2,3,4})) )
assert( (sortedGens(R,{1,1,1})) === {y-z,x-z} )
assert( (try sortedGens(R,{0,0,0}) else oops) === oops )
///

TEST ///
needsPackage"MultiplierIdeals";
debug MultiplierIdeals;
R = QQ[x,y,z];
assert( (exceptionalDivisorValuation({2,3,4},{1,1,2},x)) === 1 )
assert( (exceptionalDivisorValuation({2,3,4},{1,1,2},y)) === 1 )
assert( (exceptionalDivisorValuation({2,3,4},{1,1,2},z)) === 2 )
assert( (exceptionalDivisorValuation({2,3,4},{1,1,2},z-x^2)) === 2 )
assert( (exceptionalDivisorValuation({2,3,4},{1,1,2},(z-x^2)^3*(x+y+z))) === 7 )
assert( (exceptionalDivisorValuation({3,5,11},{7,1,2},x)) === 7 )
assert( (exceptionalDivisorValuation({3,5,11},{7,1,2},y)) === 1 )
assert( (exceptionalDivisorValuation({3,5,11},{7,1,2},z)) === 2 )
assert( (exceptionalDivisorValuation({3,5,11},{7,1,2},x^2*y-z)) === 3 )
assert( (exceptionalDivisorValuation({3,5,11},{7,1,2},
  (x^2*y-z)^2 * x * (y + z))) === 14 )
///

TEST ///
needsPackage"MultiplierIdeals";
--needsPackage "Normaliz";
debug MultiplierIdeals;
R = QQ[x,y];
assert( (monomialValuationIdeal(R,{2,3},6)) == monomialIdeal (x^3,x^2*y,y^2) )
assert( (monomialValuationIdeal(R,{2,3},1)) == monomialIdeal (x,y) )
assert( (monomialValuationIdeal(R,{2,3},0)) == monomialIdeal 1_R )
assert( (monomialValuationIdeal(R,{2,3},-1)) == monomialIdeal 1_R )

S = QQ[x,y,z];
assert( (monomialValuationIdeal(S,{2,3,4},6))
  == monomialIdeal (x^3,x^2*y,y^2,x*z,y*z,z^2) )
assert( (monomialValuationIdeal(S,{3,4,5},9))
  == monomialIdeal (x^3,x^2*y,x*y^2,y^3,x^2*z,y*z,z^2) )
assert( (monomialValuationIdeal(S,{3,5,11},0)) == monomialIdeal 1_S )
assert( (monomialValuationIdeal(S,{3,5,11},2)) == monomialIdeal (x,y,z) )
///

TEST ///
needsPackage"MultiplierIdeals";
--needsPackage "Normaliz";
debug MultiplierIdeals;
R = QQ[x,y,z];
ff = sortedGens(R,{3,4,5});
assert( (exceptionalDivisorValuationIdeal(R,ff,{1,1,2},6))
  == ideal(z^3,y^2*z^2,x*y*z^2,x^2*z^2,y^3*z,x*y^2*z,y^4,x^3*y*z,x^4*z,x^2*y^3,
  x^3*y^2,x^5*y,x^6) )
assert( (exceptionalDivisorValuationIdeal(R,ff,{2,3,4},3)) == ideal(z,y,x^2) )
S = QQ[x,y,z];
ff = sortedGens(S,{2,3,4});
assert( ( exceptionalDivisorValuationIdeal(S,ff,{1,2,2},4))
  == ideal(z^2,y*z,y^2,x^2*z,x^2*y,x^3-x*z) )
assert( ( exceptionalDivisorValuationIdeal(S,ff,{2,3,4},6))
  == ideal(z^2,y*z,x*z,y^2,x^2-z) )
assert( ( exceptionalDivisorValuationIdeal(S,ff,{2,3,4},1)) == ideal(z,y,x) )
assert( ( exceptionalDivisorValuationIdeal(S,ff,{1,2,2},6))
  == ideal(z^3,y*z^2,y^2*z,y^3,x^2*z^2,x^2*y*z,x^3*z-x*z^2,x^2*y^2,x^3*y-x*y*z,
  x^4-2*x^2*z+z^2) )
assert( ( exceptionalDivisorValuationIdeal(S,ff,{1,2,2},-1)) == ideal 1_S )
assert( ( exceptionalDivisorValuationIdeal(S,ff,{1,2,2},0)) == ideal 1_S )
///

TEST ///
needsPackage"MultiplierIdeals";
debug MultiplierIdeals;
R = QQ[x,y,z];
assert( (termIdeal(ideal{y^2-x^3})) == monomialIdeal (x^3,y^2) )
assert( (termIdeal(ideal{y^2-x^3,(x-y)^3})) == monomialIdeal (x^3,x^2*y,y^2) )
assert( (termIdeal(ideal{y^2-x^3,x*y*z+1})) == monomialIdeal 1_R )
assert( (termIdeal(ideal{0_R})) == monomialIdeal 0_R )
assert( (termIdeal(ideal{1_R})) == monomialIdeal 1_R )
///

TEST ///
needsPackage"MultiplierIdeals";
debug MultiplierIdeals;
R = QQ[x,y,z];
I = affineMonomialCurveIdeal(R,{2,3,4})
J = affineMonomialCurveIdeal(R,{3,4,5})
assert(symbolicPowerCurveIdeal(I,3) == I^3)
assert(symbolicPowerCurveIdeal(J,3) != J^3)
assert(symbolicPowerCurveIdeal(J,1) == J)
assert( (symbolicPowerCurveIdeal(J,2)) == ideal(y^4-2*x*y^2*z+x^2*z^2,
  x^2*y^3-x^3*y*z-y^2*z^2+x*z^3,x^3*y^2-x^4*z-y^3*z+x*y*z^2,
  x^5+x*y^3-3*x^2*y*z+z^3) )
assert( (symbolicPowerCurveIdeal(J,4))
  == ideal(y^8-4*x*y^6*z+6*x^2*y^4*z^2-4*x^3*y^2*z^3+x^4*z^4,
  x^2*y^7-3*x^3*y^5*z+3*x^4*y^3*z^2-x^5*y*z^3-y^6*z^2+3*x*y^4*z^3
    -3*x^2*y^2*z^4+x^3*z^5,
  x^3*y^6-3*x^4*y^4*z+3*x^5*y^2*z^2-x^6*z^3-y^7*z+3*x*y^5*z^2
    -3*x^2*y^3*z^3+x^3*y*z^4,
  x^5*y^4-2*x^6*y^2*z+x^7*z^2+x*y^7-5*x^2*y^5*z+7*x^3*y^3*z^2-3*x^4*y*z^3
    +y^4*z^3-2*x*y^2*z^4+x^2*z^5,x^7*y^3-x^8*y*z-x^4*y^4*z-x^5*y^2*z^2
    +2*x^6*z^3+y^7*z-4*x*y^5*z^2+8*x^2*y^3*z^3-5*x^3*y*z^4-y^2*z^5+x*z^6,
  x^8*y^2-x^9*z+x^4*y^5-5*x^5*y^3*z+4*x^6*y*z^2-x*y^6*z+4*x^2*y^4*z^2
    -2*x^3*y^2*z^3-x^4*z^4-y^3*z^4+x*y*z^5,x^10+2*x^6*y^3-6*x^7*y*z+x^2*y^6
    -6*x^3*y^4*z+9*x^4*y^2*z^2+2*x^5*z^3+2*x*y^3*z^3-6*x^2*y*z^4+z^6) )
assert( (symbolicPowerCurveIdeal(I,0)) == ideal 1_R )
assert( (symbolicPowerCurveIdeal(J,-1)) == ideal 1_R )
///

TEST ///
needsPackage"MultiplierIdeals";
needsPackage"Dmodules";
debug MultiplierIdeals;

R = QQ[x,y,z];
assert( (multiplierIdeal(R,{2,3,4},1)) == ideal 1_R )
assert( (multiplierIdeal(R,{2,3,4},7/6)) == ideal 1_R )
assert( (multiplierIdeal(R,{2,3,4},20/7))
  == ideal(y^2*z-x*z^2,x^2*z-z^2,y^3-x*y*z,x*y^2-z^2,x^2*y-y*z,x^3-x*z) )
assert( (multiplierIdeal(R,{3,4,5},11/5)) == ideal(y^2-x*z,x^2*y-z^2,x^3-y*z) )
I = affineMonomialCurveIdeal(R,{2,3,4})
assert(multiplierIdeal(R,{2,3,4},3/2) == Dmodules$multiplierIdeal(I,3/2))
///

TEST ///
needsPackage "MultiplierIdeals";
debug MultiplierIdeals;
assert( Qinterval( 3 , 4 , 5 ) === ( 4/1 , 13/3 , 14/3 , 5/1 ) );
assert( Qinterval( 3 , 4.5 , 5 ) === ( 14/3 , 5/1 ) );
assert( Qinterval(3,4,5, IntervalType=>"Closed") == (4/1, 13/3, 14/3, 5/1) );
assert( Qinterval(3,4,5, IntervalType=>"Open") == (13/3, 14/3) );
assert( Qinterval(3,4,5, IntervalType=>"OpenClosed") == (13/3, 14/3, 5/1) );
assert( Qinterval(3,4,5, IntervalType=>"ClosedOpen") == (4/1, 13/3, 14/3) );
R = QQ[x,y,z];
assert( jumpingDenominators(R,{2,3,6}) === {1,6} )
assert( jumpingDenominators(R,{4,5,11}) === {1,2,3,12,16} )
///

TEST ///
needsPackage "MultiplierIdeals";
R = QQ[x,y,z];
assert( (logCanonicalThreshold(R,{2,3,4})) === 11/6 )
assert( (logCanonicalThreshold(R,{3,4,5})) === 13/9 )
assert( (logCanonicalThreshold(R,{3,4,11})) === 19/12 )
///

TEST ///
  needsPackage "MultiplierIdeals";
  R = QQ[x,y,z];
  assert( (jumpingNumbers(R,{2,3,4})) == {
    {11/6,2/1},{ideal(z,y,x),ideal(-y^2+x*z,-x^2+z)}
  } )
  assert( jumpingNumbers(R,{3,4,5},IntervalType=>"ClosedOpen") == {
    {13/9, 16/9, 17/9, 2, 22/9, 5/2, 25/9, 26/9},
    {ideal(z,y,x),
     ideal(z,y,x^2),
     ideal(z,y^2,x*y,x^2),
     ideal(y^2-x*z,x^2*y-z^2,x^3-y*z),
     ideal(y^2*z-x*z^2,y^3-x*y*z,x*y^2-x^2*z,x^2*y-z^2,x^4-x*y*z),
     ideal(y^2*z-x*z^2,y^3-x*y*z,x*y^2-x^2*z,x^2*y*z-z^3,x^3*z-y*z^2,
       x^3*y-x*z^2,x^4-x*y*z),
     ideal(y^2*z-x*z^2,y^3-x*y*z,x^2*y*z-z^3,x^3*z-y*z^2,x^2*y^2-y*z^2,
       x^3*y-x*z^2,x^5-z^3),
     ideal(y^2*z-x*z^2,x^2*y*z-z^3,x^3*z-y*z^2,y^4-x^2*z^2,x*y^3-z^3,
       x^2*y^2-y*z^2,x^4*y-x^2*z^2,x^5-z^3)}
  } )
  assert( (jumpingNumbers(R,{3,4,5},Interval=>{3/2,5/2})) == {
    {16/9,17/9,2/1,22/9,5/2},
    {ideal(z,y,x^2),ideal(z,y^2,x*y,x^2),
     ideal(-y^2+x*z,-y^2*z+x*z^2,x*y^2-x^2*z,-x^2*y+z^2,-x^3+y*z),
     ideal(-y^2*z+x*z^2,-y^3+x*y*z,-x*y^2+x^2*z,-x^2*y+z^2,-y^2*z^2+x*z^3,
       x*y^2*z-x^2*z^2,-x^2*y*z+z^3,-x^3*z+y*z^2,-x^3*z+y*z^2,-x^4+x*y*z),
     ideal(y^2*z-x*z^2,y^3-x*y*z,x*y^2-x^2*z,x^2*y*z-z^3,x^3*z-y*z^2,
       x^3*y-x*z^2,x^4-x*y*z)}
  } )
///

--------------------------------------------------------------------------------
-- HYPERPLANE ARRANGEMENTS -----------------------------------------------------
--------------------------------------------------------------------------------

TEST ///
  needsPackage "MultiplierIdeals";
  R = QQ[x,y,z];
  f = toList factor((x^2 - y^2)*(x^2 - z^2)*(y^2 - z^2)*z) / first;
  A = arrangement f;
  assert(A == arrangement {z, y - z, y + z, x - z, x + z, x - y, x + y});
  assert(logCanonicalThreshold(A) == 3/7);
///

TEST ///
  needsPackage "MultiplierIdeals";
  R = QQ[x,y,z];
  f = toList factor((x^2 - y^2)*(x^2 - z^2)*(y^2 - z^2)*z) / first;
  A = arrangement f;
  assert(A == arrangement {z, y - z, y + z, x - z, x + z, x - y, x + y});
  assert(multiplierIdeal(A,3/7) == ideal(z,y,x));
///

TEST /// -- Example 5(a) of [Budur 2010]
  needsPackage "MultiplierIdeals";
  R = QQ[x,y];
  A = arrangement { y, x, y-x };
  assert( (jumpingNumbers(A)) === {
    {2/3,1/1},
    {
      ideal(y, x),
      ideal(x^2*y-x*y^2)
    }
   }
  )
///

TEST /// -- Example 5(b) of [Budur 2010]
  needsPackage "MultiplierIdeals";
  R = QQ[x,y,z];
  f = toList factor( (x^2-y^2)*(x^2-z^2) ) / first;
  A = arrangement f;
  assert( (jumpingNumbers A) === {
    {3/4,1/1},
    {ideal(z, y, x),
     ideal(x^4-x^2*y^2-x^2*z^2+y^2*z^2)}
  } )
///

TEST /// -- Example 6.3 of [Berkesch--Leykin 2010]
  needsPackage "MultiplierIdeals";
  R = QQ[x,y,z];
  ff = toList factor ( (x^2-y^2)*(x^2-z^2)*(y^2-z^2)*z ) / first;
  A = arrangement ff;
  idealList = {
    ideal(x,y,z),
    (ideal(x,y,z))^2,
    intersect(
      ideal(z,x),
      ideal(z,y),
      ideal(y+z,x+z),
      ideal(y+z,x-z),
      ideal(y-z,x+z),
      ideal(y-z,x-z)
    ),
    intersect(
      ideal(z,x),
      ideal(z,y),
      ideal(y+z,x+z),
      ideal(y+z,x-z),
      ideal(y-z,x+z),
      ideal(y-z,x-z),
      ideal(z^3,y*z^2,x*z^2,x*y*z,y^3,x^3,x^2*y^2)
    )
  };
  assert(
    jumpingNumbers(A,IntervalType=>"ClosedOpen") == {
      {3/7,4/7,2/3,6/7},
      idealList
    }
  )
///

-- Empty (zero) arrangement and unit arrangement
TEST ///
  needsPackage "MultiplierIdeals";
  R = QQ[x,y,z];
  A = arrangement({},R);
  B = arrangement({0_R},R);
  C = arrangement({1_R},R);
  assert ( trim(A) == trim(B) );
  assert ( multiplierIdeal(A,0) == ideal(1_R) );
  assert ( multiplierIdeal(A,1/2) == ideal(0_R) );
  assert ( multiplierIdeal(A,1) == ideal(0_R) );
  assert ( logCanonicalThreshold(A) == 0 );
  
  assert ( multiplierIdeal(B,0) == ideal(1_R) );
  assert ( multiplierIdeal(B,1/2) == ideal(0_R) );
  assert ( multiplierIdeal(B,1) == ideal(0_R) );
  assert ( logCanonicalThreshold(B) == 0 );
  
  assert ( multiplierIdeal(C,0) == ideal(1_R) );
  assert ( multiplierIdeal(C,1/2) == ideal(1_R) );
  assert ( multiplierIdeal(C,1) == ideal(1_R) );
  assert ( logCanonicalThreshold(C) == infinity );
///

--------------------------------------------------------------------------------
-- GENERIC DETERMINANTAL -------------------------------------------------------
--------------------------------------------------------------------------------

TEST /// -- Example 3.9 of [Johnson, 2003] (thesis)
  needsPackage "MultiplierIdeals";
  debug MultiplierIdeals;
  I = X -> genericDeterminantalSymbolicPower(X,3,4);
  J = X -> ( minors(6,X)
    + minors(3,X)*minors(5,X)
    + (minors(4,X))^2
    + (minors(3,X))^2*minors(4,X)
    + (minors(3,X))^4 );
  R = QQ[x_1..x_9];
  X = genericMatrix(R,3,3);
  assert(I(X) == J(X));
  R = QQ[x_1..x_12];
  X = genericMatrix(R,3,4);
  assert(I(X) == J(X));
  R = QQ[x_1..x_15];
  X = genericMatrix(R,3,5);
  assert(I(X) == J(X));
///

TEST /// -- Example 5.7 of [Johnson, 2003] (thesis)
  needsPackage "MultiplierIdeals";
  R = QQ[x_1..x_16];
  X = genericMatrix(R,4,4);
  J = multiplierIdeal(X,2,9);
  JJ = intersect( (minors(1,X))^3, minors(2,X) );
  assert( J == JJ );
///


-- Zero determinantal ideal and unit determinantal ideal
TEST ///
  needsPackage "MultiplierIdeals";
  R = QQ[x_1..x_6];
  X = genericMatrix(R,2,3);
  
  -- minors larger than size of matrix: zero ideal
  assert ( multiplierIdeal(X,3,0) == ideal(1_R) );
  assert ( multiplierIdeal(X,3,1/2) == ideal(0_R) );
  assert ( multiplierIdeal(X,3,1) == ideal(0_R) );
  assert ( logCanonicalThreshold(X,3) == 0 );
  
  -- size zero minors: unit ideal
  assert ( multiplierIdeal(X,0,0) == ideal(1_R) );
  assert ( multiplierIdeal(X,0,1/2) == ideal(1_R) );
  assert ( multiplierIdeal(X,0,1) == ideal(1_R) );
  assert ( logCanonicalThreshold(X,0) == infinity );
///

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- DOCUMENTATION ---------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

beginDocumentation()
document { 
  Key => MultiplierIdeals,
  Headline => "multiplier ideals",
  PARA {
    EM "MultiplierIdeals",
    " is a package for computing multiplier ideals,
    log canonical thresholds, and jumping numbers,
    using specialized routines wherever possible. "
  },
  PARA {
    "The package ",
    EM "Dmodules",
    " provides computations of multiplier ideals,
    log canonical thresholds, and jumping numbers of arbitrary ideals
    using general algorithms."
  },
  PARA {
    "This package provides alternatives for special classes of ideals,
    including monomial ideals, hyperplane arrangements,
    generic determinantal ideals,
    and binomial ideals
    (currently, ideals of curves in 3-space parametrized by monomials).
    These special computations are typically much faster than general methods
    and can often handle larger examples."
  },
--   PARA {
--     "It is hoped that future versions of this package will provide
--     computations via resolution of singularities for general ideals,
--     as well as computation for additional special classes of ideals,
--     such as plane curve singularities."
--   },
  SUBSECTION "References",
  UL {
  LI {
    "[BL] ",
    "Blickle, Manuel and Robert Lazarsfeld, ",
    EM "An informal introduction to multiplier ideals. ",
    "Trends in commutative algebra, 87-114, ",
    "Math. Sci. Res. Inst. Publ., 51, Cambridge Univ. Press, Cambridge, 2004."
  },
  LI {
    "[H] ",
    "Howald, J.A., ",
    EM "Multiplier ideals of monomial ideals. ",
    "Trans. Amer. Math. Soc. 353 (2001), no. 7, 2665-2671"
  },
  LI {
    "[J] ",
    "Johnson, Amanda, ",
    EM "Multiplier ideals of determinantal ideals. ",
    "Thesis (Ph.D.)-University of Michigan. 2003"
  },
  LI {
    "[L] ",
    "Lazarsfeld, Robert, ",
    EM "Positivity in algebraic geometry. II. ",
    "Ergebnisse der Mathematik., vol. 49, Springer-Verlag, ",
    "Berlin, 2004. Positivity for vector bundles, and multiplier ideals."
  },
  LI {
    "[M] ",
    "Mustata, Mircea, ",
    EM "Multiplier ideals of hyperplane arrangements. ",
    "Trans. Amer. Math. Soc. 358 (2006), no. 11, 5015-5023."
  },
  LI {
    "[T] ",
    "Teitler, Zach, ",
    EM "A note on Mustata's computation of multiplier ideals of hyperplane
      arrangements. ",
    "Proc. Amer. Math. Soc. 136 (2008), no. 5, 1575-1579."
  },
  LI {
    "[Th] ",
    "Thompson, Howard M., ",
    EM "Multiplier Ideals of Monomial Space Curves, ",
    "Proc. Amer. Math. Soc. Ser. B 1 (2014), 33–41."
  }
  },
  Caveat => {
    "The multiplier ideals and log canonical thresholds computed
    by this package are valid over any field in any characteristic.
    In some cases results may be computed over other coefficient rings,
    such as ZZ, but they may not represent actual
    multiplier ideals or log canonical thresholds."
  }
}

--------------------------------------------------------------------------------
-- MULTIPLIER IDEAL ------------------------------------------------------------
--------------------------------------------------------------------------------

document {
  Key => {
    multiplierIdeal,
    (multiplierIdeal,MonomialIdeal,QQ),
    (multiplierIdeal,MonomialIdeal,ZZ),
    (multiplierIdeal,CentralArrangement,List,Number),
    (multiplierIdeal,CentralArrangement,Number),
    (multiplierIdeal,Ring,List,QQ),
    (multiplierIdeal,Ring,List,ZZ),
    (multiplierIdeal,Matrix,ZZ,QQ),
    (multiplierIdeal,Matrix,ZZ,ZZ),
  },
  Headline => "multiplier ideal",
  SYNOPSIS {
    Heading => "multiplier ideal of a monomial ideal",
    Usage => "multiplierIdeal(I,t)",
    Inputs => {
      "I" => MonomialIdeal => {"a monomial ideal in a polynomial ring"},
      "t" => QQ => {"a coefficient"}
    },
    Outputs => {
      Ideal => {}
    },
    "Computes the multiplier ideal of ", TEX ///$I$///, " with coefficient ",
    TEX ///$t$///, " using Howald's Theorem and the package ", TO Normaliz, ".",
  
    EXAMPLE lines ///
      R = QQ[x,y];
      I = monomialIdeal(y^2,x^3);
      multiplierIdeal(I,5/6)
      J = monomialIdeal(x^8,y^6); -- Example 2 of [Howald 2000]
      multiplierIdeal(J,1)
    ///,
  },
  
  SYNOPSIS {
    Heading => "multiplier ideal of a hyperplane arrangement",
    Usage => "multiplierIdeal(A,m,s)",
    Inputs => {
      "A" => CentralArrangement => {"a central hyperplane arrangement"},
      "m" => List => {"a list of weights for the hyperplanes in ", TEX ///$A$///,
        " (optional)"},
      "s" => Number => {"a coefficient"}
    },
    Outputs => {
      Ideal => {}
    },
    "Computes the multiplier ideal of the ideal of ", TEX ///$A$///,
    " with coefficient ", TEX ///$s$///, " using the package ",
    TO HyperplaneArrangements,
    ".",
  
    EXAMPLE lines ///
      R = QQ[x,y,z];
      f = toList factor((x^2 - y^2)*(x^2 - z^2)*(y^2 - z^2)*z) / first;
      A = arrangement f;
      multiplierIdeal(A,3/7)
    ///
  },
  
  SYNOPSIS {
    Heading => "multiplier ideal of monomial space curve",
    Usage => "I = multiplierIdeal(R,n,t)",
    Inputs => {
      "R" => Ring,
      "n" => List => {"a list of three integers"},
      "t" => QQ
    },
    Outputs => {
      "I" => Ideal
    },
    PARA {
      "Computes the multiplier ideal of the space curve ", TEX ///$C$///,
      " parametrized by ",
      TEX ///$(t^a,t^b,t^c)$///,
      " given by ",
      TEX ///$n=(a,b,c)$///, "."
    },
    EXAMPLE lines ///
      R = QQ[x,y,z];
      n = {2,3,4};
      t = 5/2;
      I = multiplierIdeal(R,n,t)
    ///
  },
  
  SYNOPSIS {
    Heading => "multiplier ideal of a generic determinantal ideal",
    Usage => "multiplierIdeal(R,L,r,t)",
    Inputs => {
      "R" => Ring => {"a ring"},
      "L" => List => {"dimensions ", TEX ///$\{m,n\}$///, " of a matrix"},
      "r" => ZZ => {"the size of minors generating the determinantal ideal"},
      "t" => QQ => {"a coefficient"}
    },
    Outputs => {
      Ideal => {}
    },
    "Computes the multiplier ideal of the ideal of ",
    TEX ///$r \times r$///, " minors in a ", TEX ///$m \times n$///,
    " matrix whose entries are independent variables in the ring ", TEX ///$R$///,
    " (a generic matrix).",
  
    EXAMPLE lines ///
      x = symbol x;
      R = QQ[x_1..x_20];
      X = genericMatrix(R,4,5);
      multiplierIdeal(X,2,5/7)
    ///
  },
  
  SeeAlso => { MonomialIdeal }
}

--------------------------------------------------------------------------------
-- LOG CANONICAL THRESHOLD -----------------------------------------------------
--------------------------------------------------------------------------------

document {
  Key => {
    logCanonicalThreshold,
    (logCanonicalThreshold,MonomialIdeal),
    (logCanonicalThreshold,MonomialIdeal,RingElement),
    (logCanonicalThreshold,CentralArrangement),
    (logCanonicalThreshold,CentralArrangement,List),
    (logCanonicalThreshold,Ring,List),
    (logCanonicalThreshold,Matrix,ZZ),
  },
  Headline => "log canonical threshold",
  PARA {
    "The log canonical threshold of an ideal ", TEX ///$I$///, " is the infimum of ",
    TEX ///$t$///, " for which the multiplier ideal ", TEX ///$J(I^t)$///,
    " is a proper ideal.
    Equivalently it is the least nonzero jumping number."
  },
  SYNOPSIS {
    Heading => "log canonical threshold of a monomial ideal",
    Usage => "logCanonicalThreshold I",
    Inputs => {
      "I" => MonomialIdeal => {},
    },
    Outputs => {
      QQ => {}
    },
    "Computes the log canonical threshold of a monomial ideal ", TEX ///$I$///, ".",
  
    EXAMPLE lines ///
      R = QQ[x,y];
      I = monomialIdeal(y^2,x^3);
      logCanonicalThreshold(I)
      S = QQ[x,y,z];
      J = monomialIdeal(x*y^4*z^6, x^5*y, y^7*z, x^8*z^8); -- Example 7 of [Howald 2000]
      logCanonicalThreshold(J)
    ///,
  },
  
  SYNOPSIS {
    Heading => "thresholds of multiplier ideals of monomial ideals",
    Usage => "logCanonicalThreshold(I,m)",
    Inputs => {
      "I" => MonomialIdeal => {},
      "m" => RingElement => {"a monomial"}
    },
    Outputs => {
      QQ => {
        "the least ", TEX ///$t$///, " such that ", TEX ///$m$///, " is not in the ",
        TEX ///$t$///, "-th multiplier ideal of ", TEX ///$I$///
      },
      Matrix => {
        "the equations of the facets of the Newton polyhedron of ", TEX ///$I$///,
        " which impose the threshold on ", TEX ///$m$///
      }
    },
    "Computes the threshold of inclusion of the monomial ", TEX ///$m=x^v$///,
    " in the multiplier ideal ", TEX ///$J(I^t)$///, ", that is, the value ",
    TEX ///$t = sup\{ c | m lies in J(I^c) \} = min\{ c | m does not lie in J(I^c)\}$///, ". 
    In other words, ", TEX ///$(1/t)(v+(1,..,1))$///, " lies on the boundary of the Newton
    polyhedron Newt(", TEX ///$I$///, "). In addition, returns the linear inequalities for those
    facets of Newt(", TEX ///$I$///, ") which contain ", TEX ///$(1/t)(v+(1,..,1))$///, ".
    These are in the format of ", TO "Normaliz", ", i.e., a matrix ", TEX ///$(A | b)$///,
    " where the number of columns of ", TEX ///$A$///, " is the number of variables in
    the ring, ", TEX ///$b$///, " is a column vector, and the inequality on the column
    vector ", TEX ///$v$///, " is given by ", TEX ///$Av+b \geq 0$///, ", entrywise.
    As a special case, the log canonical threshold is the threshold of the
    monomial ", TEX ///$1_R = x^0$///, ".",
  
    EXAMPLE lines ///
      R = QQ[x,y];
      I = monomialIdeal(x^13,x^6*y^4,y^9);
      logCanonicalThreshold(I,x^2*y)
      J = monomialIdeal(x^6,x^3*y^2,x*y^5); -- Example 6.7 of [Howald 2001] (thesis)
      logCanonicalThreshold(J,1_R)
      logCanonicalThreshold(J,x^2)
    ///
  },
  
  SYNOPSIS {
    Heading => "log canonical threshold of a hyperplane arrangement",
    Usage => "logCanonicalThreshold A",
    Inputs => {
      "A" => CentralArrangement => {"a central hyperplane arrangement"}
    },
    Outputs => {
      QQ => {}
    },
    "Computes the log canonical threshold of a hyperplane arrangement ",
    TEX ///$A$///, ".",
  
    EXAMPLE lines ///
      R = QQ[x,y,z];
      f = toList factor((x^2 - y^2)*(x^2 - z^2)*(y^2 - z^2)*z) / first;
      A = arrangement f;
      logCanonicalThreshold(A)
    ///
  },
  
  SYNOPSIS {
    Heading => "log canonical threshold of monomial space curves",
    Usage => "logCanonicalThreshold(R,n)",
    Inputs => {
      "R" => Ring,
      "n" => List => {"a list of three integers"}
    },
    Outputs => {
      "logCanonicalThreshold" => QQ
    },
    PARA {
      "Computes the log canonical threshold of the ideal ",
      TEX ///$I$///, " of a space curve parametrized by ",
      TEX ///$u \to (u^a,u^b,u^c)$///, "."
    },
    
    EXAMPLE lines ///
      R = QQ[x,y,z];
      n = {2,3,4};
      logCanonicalThreshold(R,n)
    ///
  },
  
  SYNOPSIS {
    Heading => "log canonical threshold of a generic determinantal ideal",
    Usage => "multiplierIdeal(L,r)",
    Inputs => {
      "L" => List => {"dimensions ", TEX ///$\{m,n\}$///, " of a matrix"},
      "r" => ZZ => {"the size of minors generating the determinantal ideal"}
    },
    Outputs => {
      QQ => {}
    },
    "Computes the log canonical threshold of the ideal of ",
    TEX ///$r \times r$///, " minors in a ", TEX ///$m \times n$///,
    " matrix whose entries are independent variables ",
    "(a generic matrix).",
    
    PARA {
      "lct of ideal of 2-by-2 minors of 4-by-5 matrix:",
    },
    EXAMPLE lines ///
      x = getSymbol "x";
      R = QQ[x_1..x_20];
      X = genericMatrix(R,4,5);
      logCanonicalThreshold(X,2)
    ///,
    "We produce some tables of lcts:",
    EXAMPLE {///
      lctTable = (M,N,r) -> (
        x = getSymbol "x";
        R := QQ[x_1..x_(M*N)];
        netList (
        prepend( join({"m\\n"}, toList(3..M)),
        for n from 3 to N list (
          prepend(n,
          for m from 3 to min(n,M) list (
            logCanonicalThreshold(genericMatrix(R,m,n),r)
          ))
        ))
      ));
    ///},
    "Table of LCTs of ideals of 3-by-3 minors of various size matrices
    (Table A.1 of [Johnson, 2003] (dissertation))",
    EXAMPLE {"lctTable(6,10,3)"},
    "Table of LCTs of ideals of 4-by-4 minors of various size matrices
    (Table A.2 of [Johnson, 2003] (dissertation))",
    EXAMPLE {"lctTable(8,14,4)"}
  },
  
  SeeAlso => {
    jumpingNumbers
  }
}

--------------------------------------------------------------------------------
-- JUMPING NUMBERS -------------------------------------------------------------
--------------------------------------------------------------------------------

document {
  Key => {
    jumpingNumbers,
    (jumpingNumbers,CentralArrangement),
    (jumpingNumbers,MonomialIdeal),
    (jumpingNumbers,Sequence),
  },
  Headline => "jumping numbers",
  PARA {
    "Jumping numbers of an ideal ", TEX ///$I$///, " are those real numbers ",
    TEX ///$t$///,
    " at which the multiplier ideal ", TEX ///$J(I^t)$///,
    ", as a function of the parameter ", TEX ///$t$///, ", changes.
    More precisely, ", TEX ///$t$///, " is a jumping number if ",
    TEX ///$J(I^t)$///,
    " is different from ",
    TEX ///$J(I^{t-\epsilon})$///,
    " for all ",
    TEX ///$\epsilon > 0$///,
    ". The jumping numbers form a discrete sequence of rational numbers.
    Thus ",
    TEX ///$t_1, t_2$///,
    " are two consecutive jumping numbers of ", TEX ///$I$///, " when ",
    TEX ///$J(I^t) = J(I^{t_1})$///,
    " for all ", TEX ///$t_1 \leq t < t_2$///,
    " and ", TEX ///$J(I^t) \neq J(I^{t_1})$///,
    " for ", TEX ///$t < t_1$///,
    " or ", TEX ///$t_2 \leq t$///, "."
  },
  PARA {
    "The jumpingNumbers command determines the jumping numbers
    of an ideal along with the multiplier ideals at those jumping numbers.
    By definition, the multiplier ideals are then determined at the
    intermediate parameter values."
  },
  PARA {
    "The jumpingNumbers command can handle any input that the multiplierIdeals
    command can handle (but omit the rational number argument of
    multiplierIdeals). That is, jumpingNumbers can handle the following inputs:
    a hyperplane arrangement; a hyperplane arrangement with a list of
    multiplicities; a monomial ideal; a monomial space curve
    (specified by giving a ring together with a list of exponents);
    a generic determinantal ideal
    (specified by giving a generic matrix together with a size of minors)."
  },
  EXAMPLE lines ///
    R = QQ[x,y,z,w];
    I = monomialIdeal(x*y, x*z, y*z, y*w, z*w^2);
    jumpingNumbers(I)
  ///,
  PARA {
    "By default, jumpingNumbers looks for jumping numbers in a closed
    interval ",
    TEX ///$[a,b]$///, " where ", TEX ///$a$///, " is the log canonical threshold
    of the ideal and ", TEX ///$b$///, " is a sufficiently large value to ensure
    that Skoda periodicity holds, that is, ",
    TEX ///$J(I^t) = I J(I^{t-1})$///, " for ", TEX ///$t \geq b$///, ".
    (In particular, the multiplier ideals and jumping numbers are determined
    for all ", TEX ///$t$///, " by the output of this command.)
    The user may specify a different interval via the optional arguments ",
    TO "Interval", " and ", TO "IntervalType", "."
  },
  EXAMPLE lines ///
    R = QQ[x,y,z,w];
    I = monomialIdeal(x*y, x*z, y*z, y*w, z*w^2);
    jumpingNumbers(I,Interval=>{2,3},IntervalType=>"OpenClosed")
  ///,
  SUBSECTION "References",
  UL {
  LI {
    "[ELSV] ",
    "Ein, Lawrence, Robert Lazarsfeld, Karen E. Smith, and Dror Varolin, ",
    EM "Jumping coefficients of multiplier ideals. ",
    "Duke Math. J. 123 (2004), no. 3, 469-506."
  }
  },
  SeeAlso => {
    logCanonicalThreshold
  }
}


--------------------------------------------------------------------------------
-- OPTIONAL ARGUMENTS ----------------------------------------------------------
--------------------------------------------------------------------------------

document {
  Key => {
    Interval
  },
  Headline => "name for an optional argument"
}

document {
  Key => {
    IntervalType
  },
  Headline => "name for an optional argument"
}

document {
  Key => {
    [jumpingNumbers, Interval],
    [jumpingNumbers, IntervalType]
  },
  Headline => "interval for jumping numbers",
  PARA {
    "Specify the interval in which to search for jumping numbers.
    IntervalType may be \"Closed\" (default), \"Open\",
    \"ClosedOpen\", or \"OpenClosed\"."
  },
  EXAMPLE lines ///
    R = QQ[x,y];
    I = monomialIdeal(y^2,x^3);
    jumpingNumbers(I) -- default: interval [5/6, 2]
    jumpingNumbers(I,Interval=>{1,3/2}) -- [1,3/2]
    jumpingNumbers(I,IntervalType=>"Open") -- (5/6,2)
    jumpingNumbers(I,Interval=>{1,3/2},IntervalType=>"ClosedOpen") -- [1,3/2)
  ///
}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- THE END ---------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

end



restart
loadPackage "MultiplierIdeals"
installPackage oo

check oo

