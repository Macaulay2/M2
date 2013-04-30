-- -*- coding: utf-8 -*-

{*
Copyright 2010, 2011 Zach Teitler

You may redistribute this file under the terms of the GNU General Public
License as published by the Free Software Foundation, either version 2 of
the License, or any later version.
*}

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
------------------------ MONOMIAL MULTIPLIER IDEALS ------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

newPackage(
  "MonomialMultiplierIdeals",
      Version => "0.6.1", 
      Date => "July 21, 2011",
      Authors => {
       {Name => "Zach Teitler", Email => "zteitler@member.ams.org"}
      },
      HomePage => "http://math.boisestate.edu/~zteitler/",
      Headline => "A Macaulay2 package to compute multiplier ideals of monomial ideals",
      AuxiliaryFiles => false,
      DebuggingMode => false
      )

-- MonomialMultiplierIdeals
-- Compute multiplier ideals of monomial ideals, using Normaliz to implement Howald's Theorem.
-- v0.1, May 1, 2010: Initial version.
-- v0.2, Aug 26, 2010: Lots of improvements.
-- v0.3, Sept. 17, 2010: Major improvements and cleanup. 
-- v0.4, Nov. 28, 2010: Use new versions of M2 and Normaliz; configurable temporary directory.
--    First limited distribution!
-- v0.5, May 25, 2011: Rename package to use correct capitalization convention. Strip out code
--    for dealing with temporary directories; just use M2's built in facilities.
-- v0.6, June 4, 2011: Add "monomialThreshold" feature: threshold of arbitrary monomials
--    (lct = threshold of 1), together with list of facets imposing the threshold.
--    Feature suggested by Bernd Sturmfels.
-- v0.6.1, July 21, 2011: Clean up the way the "bump" is formed in computing the multiplier ideal
--    (no longer incorrectly "bump" the row corresponding to the upper halfspace);
--    also, number tests correctly (starting from 0)


----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
------------------------ EXPORTS -------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

export {
  monomialMultiplierIdeal,
  monomialLCT,
  monomialThreshold,
  monomialJumpingNumbers}
exportMutable {}


----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
------------------------ PACKAGES ------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------


needsPackage("Normaliz");
-- Set Normaliz version to "normbig", arbitrary-precision arithmetic (vs. "norm64", 64-bit)
-- Format of command in previous versions (Macaulay2 1.3 and pre; Normaliz 2.1 and pre)
-- setNmzVersion("normbig");
-- Format of command as of Macaulay2 1.4, Normaliz 2.5:
-- nmzVersion="normbig";
-- Format of command as of Normaliz 2.7:
setNmzOption("bigint",true);

needsPackage("ReesAlgebra");



----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
------------------------ METHODS -------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------


-- NewtonPolyhedron
-- compute a matrix A such that Ax >= 0 defines the cone over
-- the Newton polyhedron of a monomial ideal
-- (ie Newt(I) is placed at height 1)
-- Uses Normaliz
NewtonPolyhedron = method();
NewtonPolyhedron (MonomialIdeal) := (I) -> (
  
  R := ring I;
  use R;
  nmzFilename = temporaryFileName() ;
  setNmzOption("supp",true);
  
  -- Compute equations of supporting hyperplanes of (cone over) Newt(I)
  -- new version of Normaliz/M2 interface no longer exports mons2intmat function
  -- following code 'matrix(I / exponents / flatten)' is paraphrased from Normaliz.m2 source
--  normaliz( mons2intmat(I), 3 ); -- worked with Normaliz.m2, version 0.2.1
  normaliz( matrix(I / exponents / flatten) , 3 ); -- works with Normaliz.m2 version 0.2.1 or 2.0
  M := readNmzData("sup");
  
  -- Clean up tmp files, options
  setNmzOption("normal",true);
  rmNmzFiles();
  
  return M;
  
);

-- monomialMultiplierIdeal
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
---- These define *coordinate* facets of Newt(I) (ie, facets of Newt(I) contained
---- in facets of the positive orthant). All other rows define *non-coordinate*
---- facets.
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
---- non-coordinate faces.) Here "interior" means: relative to the positive orthant.
---- In other words, we are removing the part of the boundary of t*Newt(I)
---- which is in the interior of the positive orthant.
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

monomialMultiplierIdeal = method();
monomialMultiplierIdeal (MonomialIdeal, ZZ) := (I,t) -> monomialMultiplierIdeal(I,promote(t,QQ));
monomialMultiplierIdeal (MonomialIdeal, QQ) := (I,t) -> (
  
  R := ring I;
  use R;
  local multIdeal;
  
  
  if ( t <= 0 ) then (
    
    multIdeal = monomialIdeal(1_R) ;
  
  ) else if ( t >= keynumber I ) then (
    
    s := 1 + floor(t-keynumber(I));
    multIdeal = I^s*monomialMultiplierIdeal(I,t-s) ;
  
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
    -- (except we do end up bumping the row (0,..,0,1); but that's okay, no harm)
    bump := apply(toList(0..<m) ,
      i -> (  if ( M_(i,n-1) >= 0 ) then ( return 0; ) else ( return 1; )  ) );
    -- now bump has a 0 in rows corresponding to coordinate facets, 1 in other rows
  
    -- "Bump" t*Newt(I): push nontrivial facets in by "epsilon" (which is 1)
    M2 := M1 - ( matrix(toList(m:toList((n-1):0))) | transpose matrix({bump}) );
    
    -- Semigroup of lattice points inside "bumped" region (i.e., interior of t*Newt(I))
    nmzOut := normaliz(M2,4);
    M3 := nmzOut#"gen";
    
    -- Ideal generated by those lattice points
    -- Normaliz.m2 version 0.2.1: exports an 'intmat2mons' command
    -- the following works in Normaliz.m2 version0.2.1:
--    J := intmat2mons(M3,R,1);
    -- Normaliz.m2 version 2.0 no longer exports that command --- it's internal, we can't use it :(
    -- so I wrote my own... somewhat copied from Normaliz.m2
    J := intmat2monomIdeal(M3,R,1);
    
    -- Howald's translation by (1,1,...,1) is same as colon with product of variables
    multIdeal = monomialIdeal quotient(J, product(flatten entries vars R)) ;
    
    -- Clean up tmp files
    rmNmzFiles();
    
  );
  
  return multIdeal;

);


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
intmat2monomIdeal ( Matrix, Ring, ZZ ) := (M,R,d) -> intmat2monomIdeal(M,R,d,numColumns(M)-1);
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




-- monomialLCT: lct of monomial ideal
monomialLCT = method();
monomialLCT (MonomialIdeal) := (I) -> (
  
--  M := NewtonPolyhedron(I);
--  m := numRows M;
--  n := numColumns M;
--  
--  lctList := {};
--  
--  local i;
--  for i from 0 to m-1 do (
--    s := sum( toList(0..(n-2)) , j -> M_(i,j) );
--    if ( M_(i,n-1) != 0 and s != 0 ) then (
--      lctList = append(lctList , -1*s / M_(i,n-1));
--    );
--  );
--  
--  return min(lctList);
--);

  return first monomialThreshold ( I , 1_(ring(I)) ) ;
);

  

-- monomialThreshold
-- threshold of inclusion in a multiplier ideal
-- input:
--  1. a monomial ideal I
--  2. a monomial x^v, or exponent vector v
-- output: a pair
--  1. a rational number t, defined by
--        t = sup { c : x^v is in the c'th multiplier ideal of I }
--     the threshold (of inclusion of x^v in the multiplier ideal of I).
--  2. a matrix (A' | -b') consisting of those rows of the defining matrix of Newt(I)
--     which impose the threshold on x^v.
--  In other words, the line joining the origin to the vector (v+(1,..,1)) hits the boundary of Newt(I)
--  at (1/t)*(v+(1,..,1)), and the vector (1/t)*(v+(1,..,1)) lies on the facets defined by the rows of (A' | -b')
--  (via: (A'|-b')(w|1)^transpose >= 0 )
monomialThreshold = method();
monomialThreshold (MonomialIdeal , RingElement) := (I , m) -> (
  if ( leadMonomial(m) != m ) then (
    error("Second input must be a monomial (input was ",m,")");
  ) else (
    return monomialThreshold(I,first exponents m);
  );
  return 0;
);
monomialThreshold (MonomialIdeal , List) := (I , v) -> (
  
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




-- keynumber: 'key number' of an ideal,
-- a la Hochster-Huneke:
-- should be keynumber=min(ambient dimension, numgens I, analyticSpread I) = analyticSpread I
-- v0.2b: keynumber = ambient dimension = numColumns vars ring I
-- v0.2c: keynumber = analyticSpread
keynumber = (I) -> (
--  return numColumns vars ring I;
  return analyticSpread(I); -- defined in package 'ReesAlgebra'
);


jumpingDenominators = method();
jumpingDenominators ( MonomialIdeal ) := (I) -> (
  
  denomList := {};
  
  M := NewtonPolyhedron(I);
  m := numRows M;
  n := numColumns M;
  
  local i;
  for i from 0 to m-1 do (
    s := sum( toList(0..(n-2)) , j -> M_(i,j) );
    if ( M_(i,n-1) != 0 and s != 0 ) then (
      denomList = append(denomList , -M_(i,n-1));
    );
  );
  
  return denomList;
);

-- Qinterval
-- give all rational numbers k/denom in the interval [a, b]
Qinterval = method();
Qinterval ( ZZ , Number , Number ) := ( denom, a, b ) -> (
  start := ceiling(denom*a);
  end := floor(denom*b);
  
  L := apply(start..end , k -> promote(k/denom,QQ));
  
  return toList L;
);
  
-- potentialJumpingNumbers
-- give a sorted list of all potential jumping numbers
-- in the interval [a,b]
-- Default: [a,b] = [monomialLCT(I),keynumber(I)]
potentialJumpingNumbers = method();
potentialJumpingNumbers ( MonomialIdeal ) := (I) -> potentialJumpingNumbers(I,monomialLCT(I),keynumber(I));
potentialJumpingNumbers ( MonomialIdeal , Number , Number ) := (I , Left, Right) -> (
  
  a := max(Left, monomialLCT(I));
  b := Right;
  
  -- empty interval?
  if ( a > b ) then (
    return { };
  );
  
  denoms := jumpingDenominators(I);
  jnList := set {};
  
  local i;
  for i from 0 to (#denoms-1) do (
    jnList = jnList + set Qinterval( denoms#i , a , b );
  );
  
  jnList = sort toList jnList;
  
  return jnList;
);


-- monomialJumpingNumbers
-- return a list {jumpingNumbers, multiplierIdeals}
-- where for jumpingNumbers#i <= c < jumpingNumbers#(i+1), J(I^c) = multiplierIdeals#i
-- Finds jumping numbers in interval [a,b]
-- Default: [a,b] = [monomialLCT(I),keynumber(I)]
monomialJumpingNumbers = method();
monomialJumpingNumbers ( MonomialIdeal ) := (I) -> monomialJumpingNumbers(I,monomialLCT I,keynumber I);
monomialJumpingNumbers ( MonomialIdeal , ZZ , ZZ ) := (I,a,b) -> monomialJumpingNumbers(I,promote(a,QQ),promote(b,QQ));
monomialJumpingNumbers ( MonomialIdeal , QQ , ZZ ) := (I,a,b) -> monomialJumpingNumbers(I,promote(a,QQ),promote(b,QQ));
monomialJumpingNumbers ( MonomialIdeal , ZZ , QQ ) := (I,a,b) -> monomialJumpingNumbers(I,promote(a,QQ),promote(b,QQ));

monomialJumpingNumbers ( MonomialIdeal , QQ , QQ ) := (I , Left , Right) -> (
  
  R := ring I;
  use R;
  
  lct := monomialLCT(I);
  
  pjn := potentialJumpingNumbers(I, Left, Right);
  
  -- Empty list?
  if ( #pjn == 0 ) then (
    return { { }, { } }; -- no jumping numbers, no multiplier ideals
  );
  
  local prev;
  local next;
  local jumpingNumbers;
  local multiplierIdeals;
  
  -- Figure out whether pjn#0 is a jumping number:
  -- We know pjn#0 >= lct.
  -- If pjn#0 == lct, then it's definitely a jumping number
  -- Otherwise, need to actually check:
  -- we want to compare J(I^p) and J(I^(p-epsilon)) (where p=pjn#0)
  -- We don't know how small epsilon needs to be,
  -- so find the greatest potential jumping number less than p
  -- and use that for p-epsilon
  if ( (pjn#0) == lct ) then (
    jumpingNumbers = { lct };
    prev = monomialMultiplierIdeal(I,lct);
    next = prev;
    multiplierIdeals = { prev };
  ) else (
    pjn2 := potentialJumpingNumbers(I, lct, pjn#0 );
    -- pjn2#-1 = pjn#0
    -- The greatest potential jumping number less than p is pjn2#-2
    prev = monomialMultiplierIdeal(I,pjn2#-2);
    next = monomialMultiplierIdeal(I,pjn#0);
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
    next = monomialMultiplierIdeal(I,pjn#i);
    
    if ( prev != next ) then (
      jumpingNumbers = append( jumpingNumbers , pjn#i );
      multiplierIdeals = append( multiplierIdeals , next );
    );
  );
  
  return {jumpingNumbers, multiplierIdeals};
);



----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
------------------------ DOCUMENTATION -------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

beginDocumentation()
document { 
  Key => MonomialMultiplierIdeals,
  Headline => "A package for computing multiplier ideals of monomial ideals",
  EM "MonomialMultiplierIdeals", " is a package to compute multiplier ideals of monomial ideals."
}

document {
  Key => {monomialMultiplierIdeal,
         (monomialMultiplierIdeal, MonomialIdeal, QQ),
         (monomialMultiplierIdeal, MonomialIdeal, ZZ)
         },
  Headline => "multiplier ideal of a monomial ideal",
  Usage => "monomialMultiplierIdeal(I,t)",
  Inputs => {
    "I" => MonomialIdeal => {"a monomial ideal in a polynomial ring"},
    "t" => QQ => {"a coefficient"}
  },
  Outputs => {
    MonomialIdeal => {}
  },
  "Computes the multiplier ideal of I with coefficient t ",
  "using Howald's Theorem and the package ", TO Normaliz, ".",
  
  EXAMPLE lines ///
R = QQ[x,y];
I = monomialIdeal(y^2,x^3);
monomialMultiplierIdeal(I,5/6)
  ///,
  
  SeeAlso => { "monomialLCT", "monomialJumpingNumbers" }
}


document {
  Key => {monomialLCT, (monomialLCT, MonomialIdeal)},
  Headline => "log canonical threshold of a monomial ideal",
  Usage => "monomialLCT I",
  Inputs => {
    "I" => MonomialIdeal => {},
  },
  Outputs => {
    QQ => {}
  },
  "Computes the log canonical threshold of a monomial ideal I ",
  "(the least positive value of t such that the multiplier ideal ",
  "of I with coefficient t is a proper ideal).",
  
  EXAMPLE lines ///
R = QQ[x,y];
I = monomialIdeal(y^2,x^3);
monomialLCT(I)
  ///,
  
  SeeAlso => { "monomialMultiplierIdeal", "monomialThreshold" }
}


document {
  Key => { monomialThreshold },
  Headline => "thresholds of multiplier ideals of monomial ideals",
  Usage => "monomialThreshold(I,m)",
  Inputs => {
    "I" => MonomialIdeal => {},
    "m" => RingElement => {"a monomial"}
  },
  Outputs => {
    QQ => {"the least t such that m is not in the t-th multiplier ideal of I"},
    Matrix => {"the equations of the facets of the Newton polyhedron of I which impose the threshold on m"}
  },
  "Computes the threshold of inclusion of the monomial m=x^v in the multiplier ideal J(I^t), ",
  "that is, the value t = sup{ c | m lies in J(I^c) } = min{ c | m does not lie in J(I^c)}. ",
  "In other words, (1/t)*(v+(1,..,1)) lies on the boundary of the Newton polyhedron Newt(I). ",
  "In addition, returns the linear inequalities for those facets of Newt(I) which contain (1/t)*(v+(1,..,1)). ",
  "These are in the format of ", TO "Normaliz", ", i.e., a matrix (A | b) where the number of columns of A is ",
  "the number of variables in the ring, b is a column vector, and the inequality on the column ",
  "vector v is given by Av+b >= 0, entrywise. ",
  "As a special case, the log canonical threshold is the threshold of the monomial 1_R = x^0.",
  
  EXAMPLE lines ///
R = QQ[x,y];
I = monomialIdeal(x^13,x^6*y^4,y^9);
monomialThreshold(I,x^2*y)
  ///,
  
  SeeAlso => { "monomialLCT" }
}

document { Key => { (monomialThreshold, MonomialIdeal, RingElement) } }
document { Key => { (monomialThreshold, MonomialIdeal, List) } }



document {
  Key => {monomialJumpingNumbers,
          (monomialJumpingNumbers, MonomialIdeal),
          (monomialJumpingNumbers, MonomialIdeal, ZZ, ZZ),
          (monomialJumpingNumbers, MonomialIdeal, ZZ, QQ),
          (monomialJumpingNumbers, MonomialIdeal, QQ, ZZ),
          (monomialJumpingNumbers, MonomialIdeal, QQ, QQ)
         },
  Headline => "jumping numbers of a monomial ideal and the corresponding multiplier ideals",
  Usage => "(jn, mi) = monomialJumpingNumbers I, (jn, mi) = monomialJumpingNumbers(I,a,b)",
  Inputs => {
    "I" => MonomialIdeal => {},
  },
  Outputs => {
    "jn" => List => {"the list of jumping numbers"},
    "mi" => List => {"the list of corresponding multiplier ideals"}
  },
  
  "Computes the jumping numbers t of a monomial ideal I ",
  "(those values of the parameter at which the multiplier ideal changes) ",
  "along with the corresponding multiplier ideals J(I^t) ",
  "in the interval [a,b] (default: [ monomialLCT(I), analyticSpread(I) ]). ",
  
  EXAMPLE lines ///
R = QQ[x,y];
I = monomialIdeal(y^2,x^3);
monomialJumpingNumbers(I)
  ///,
  
  SeeAlso => { "monomialMultiplierIdeal" }
}


----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
------------------------ TESTS ---------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------


-- Test 0
-- Compute a NewtonPolyhedron and intmat2monomialIdeal:
-- go from Ideal -> Polyhedron -> Ideal, see if it is the same again
TEST ///
  needsPackage "Normaliz";
  needsPackage "MonomialMultiplierIdeals";
  debug MonomialMultiplierIdeals;
  R := QQ[x,y,z];
  use R;
  I := monomialIdeal(x*z^2,y^3,y*z^3,y^2*z^2,x*y^2*z,x^2*y*z,x^3*z,x^2*y^2,x^4*y,x^5,z^6);
  -- this I is integrally closed!
  M1 := NewtonPolyhedron(I); -- integer matrix (A|b) s.t. Newt(I) = {Ax \geq b}
  nmzOut := normaliz(M1,4);
  M2 := nmzOut#"gen"; -- integer matrix whose rows (minimally) generate semigroup 
   -- of lattice points in {Ax \geq b}, where M1 = (A|b)
  J := intmat2monomIdeal(M2,R,1); -- integer matrix -> ideal
  assert ( I === J );
///


-- Test 1
-- Test some of the underlying routines
-- jumpingDenominators
-- potentialJumpingNumbers
TEST ///
  needsPackage "Normaliz";
  needsPackage "MonomialMultiplierIdeals";
  debug MonomialMultiplierIdeals;
  assert ( Qinterval( 3 , 4 , 5 ) === { 4/1 , 13/3 , 14/3 , 5/1 } );
  assert ( Qinterval( 3 , 4.5 , 5 ) === { 14/3 , 5/1 } );
  
  R := QQ[x,y];
  I := monomialIdeal( y^3 , y*x^2 , x^5 ) ;
  assert ( sort jumpingDenominators(I) === { 3 , 5 } );
  
  assert ( potentialJumpingNumbers(I) === {2/3, 4/5, 1/1, 6/5, 4/3, 7/5, 8/5, 5/3, 9/5, 2/1} );
  assert ( potentialJumpingNumbers(I , 3 , 4) === {3/1, 16/5, 10/3, 17/5, 18/5, 11/3, 19/5, 4/1} );
  
  -- A couple of "edge cases"
  assert ( potentialJumpingNumbers(I , 0 , 1/2) === {} );
  assert ( potentialJumpingNumbers(I , 1 , 1) === {1/1} );
///


-- Test 2
-- Compute some LCTs of diagonal monomial ideals
TEST ///
  needsPackage "MonomialMultiplierIdeals";
  R := QQ[x_1..x_5];
  use R;
  for a from 1 to 6 do (
    for b from a to 6 do (
      for c from b to 6 do (
        for d from c to 6 do (
          for e from d to 6 do (
            I := monomialIdeal(x_1^a,x_2^b,x_3^c,x_4^d,x_5^e);
            l := 1/a+1/b+1/c+1/d+1/e;
            assert ( monomialLCT(I) === l );
          );
        );
      );
    );
  );
///


-- Test 3
-- A very small ordinary computation
TEST ///
  needsPackage "MonomialMultiplierIdeals";
  R := QQ[x,y];
  use R;
  I := monomialIdeal(y^2,x^3);
  JN := monomialJumpingNumbers(I);
  assert ( JN#0 === {5/6, 7/6, 4/3, 3/2, 5/3, 11/6, 2/1} );
  assert ( JN#1 === {monomialIdeal (x,y), monomialIdeal (x^2,y), monomialIdeal (x^2,x*y,y^2),
                      monomialIdeal (x^3,x*y,y^2), monomialIdeal (x^3,x^2*y,y^2),
                      monomialIdeal (x^4,x^2*y,x*y^2,y^3),
                      monomialIdeal (x^4,x^3*y,x*y^2,y^3)} );
///


-- Test 4
-- A small ordinary computation
TEST ///
  needsPackage "MonomialMultiplierIdeals";
  R := QQ[x,y,z];
  use R;
  I := monomialIdeal( x*y , x*z , y*z );
  JN1 := monomialJumpingNumbers(I);
  assert ( JN1#0 === {3/2, 2/1, 5/2, 3/1} );
  assert ( JN1#1 === {monomialIdeal (x,y,z), monomialIdeal (x*y,x*z,y*z),
                      monomialIdeal (x^2*y,x*y^2,x^2*z,x*y*z,y^2*z,x*z^2,y*z^2),
                      monomialIdeal (x^2*y^2,x^2*y*z,x*y^2*z,x^2*z^2,x*y*z^2,y^2*z^2)} );
  
  JN2 := monomialJumpingNumbers(I^2);
  assert ( JN2#0 === {3/4, 1/1, 5/4, 3/2, 7/4, 2/1, 9/4, 5/2, 11/4, 3/1} );
  
  II := I^2 + monomialIdeal(x*y*z);
  assert ( II === monomialIdeal( x^2*y^2 , x^2*z^2 , y^2*z^2 , x*y*z ) ) ;
  JN3 := monomialJumpingNumbers(II);
  assert ( JN3#0 === {1/1, 3/2, 2/1} );
  assert ( JN3#1 === {monomialIdeal (x*y,x*z,y*z),
                      monomialIdeal (x^2*y^2,x*y*z,x^2*z^2,y^2*z^2),
                      monomialIdeal (x^3*y^3,x^2*y^2*z,x^2*y*z^2,x*y^2*z^2,x^3*z^3,y^3*z^3)} );
///


-- Test 5
-- Threshold computations
TEST ///
  needsPackage "MonomialMultiplierIdeals";
  R := QQ[x,y];
  use R;
  I := monomialIdeal( y^2 , x^3 );
  assert ( monomialThreshold( I , 1_R ) === (5/6,map(ZZ^1,ZZ^3,{{2, 3, -6}})) );
  assert ( monomialThreshold( I , x ) === (7/6,map(ZZ^1,ZZ^3,{{2, 3, -6}})) );
  I = monomialIdeal( x^3 , x*y , y^4 );
  assert ( monomialThreshold( I , 1_R ) === (1/1,map(ZZ^2,ZZ^3,{{1, 2, -3}, {3, 1, -4}})) );
  assert ( monomialThreshold( I , x ) === (4/3,map(ZZ^1,ZZ^3,{{1, 2, -3}})) );
///


end
