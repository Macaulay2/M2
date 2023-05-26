
-- An "annotated ideal" is a hash table with keys:
--   Ideal:  I
--   Linears: L
--   NonzeroDivisors: NZ
--   Inverted: inverteds
--   where
--     (a) I is an ideal (in a subset of the variables)
--     (b) L is a list of (x, g, f), where
--     (c) x is a variable (not appearing in I at all)
--     (d) g is a monic poly not involving x
--     (e) f = xg-h is in the original ideal (leadTerm f is not nec leadTerm(xg))
--     (f) h does not involve x.
--   NZ: list of known nonzero-divisors.  This is used only for performance:
--     once we know that e.g. A.Ideal : f == A.Ideal, then f can be placed on this list.
--   inverteds: Elements that have been 'inverted' in the calculation.  Need to saturate
--     with respect to these when reconstructing the associated ideal, assuming that
--     the ideal is not known to be prime already.
-- HOWEVER, if the keys IndependentSet, LexGBOverBase exist
--   then I is only contained in the associated ideal.
--   These keys contain the following info:
--     A.IndependentSet   This is a triple (basevars,S,SF) where S,SF are returned from makeFiberRings
--     A.LexGBOverBase  GB of ISF over SF
--     If one of these flags is set, both are, and the resulting ideal is equidimensional.
-- Other keys:
--   Finished Flags: if any of these flags exists, then that split
--   technique would have no further effect on the annotated ideal A.
--    A.Birational
--    A.Linear
--    A.Factorization
--    A.IndependentSet
--    A.SplitTower
--    A.DecomposeMonomials
--    A.Trim
--    A.LexGBSplit is set once LexGBOverBase consists of irred polynomials over the base field.
--   A.isPrime: possible values: "YES", "NO", "UNKNOWN".  Usually "YES" or "UNKNOWN".
-- The associated ideal consists of 3 parts, with a potential saturation step:
--   (a) the linear polynomials in L
--   (b) the ideal I
--   (c) if LexGBOverBase is a key, then the contraction of (ideal A.LexGBOverBase) to the polynomial ring
-- the saturation is with respect to all g for each (x,g,f) in L.
-- See "ideal AnnotatedIdeal" for the exact formula.

AnnotatedIdeal = new Type of MutableHashTable
AnnotatedIdeal.synonym = "annotated ideal"

-- Constructor
annotatedIdeal = method()
annotatedIdeal(Ideal, List, List, List) := AnnotatedIdeal => (I, linears, nzds, inverted) -> (
    -- See above for the definition of an annotated ideal and its keys
    -- The arguments are named the same thing as in that description
    -- nzds is a list of polynomials which are nzds's of the associated ideal
    -- inverted is a list of elements such that we ignore the minimal primes
    --   that contain any of these elements
    -- The associated ideal is:
    --   saturate(I + ideal(linears/last), product unique join((linears / (x -> x#1)),inverted))
    new AnnotatedIdeal from {
        symbol Ideal => I,
        symbol Linears => linears,
        symbol NonzeroDivisors => monicUniqueFactors nzds,
        symbol Inverted => monicUniqueFactors inverted
        }
    )


net  AnnotatedIdeal := I -> peek I
ring AnnotatedIdeal := I -> ring I.Ideal

---------------------------------------------
--- Helper functions
---------------------------------------------

-- Question:
-- What if we want to contract away only some of the basevars,
-- not all of them? Will this ever be the case?
-- TODO NOTE: the saturate here should be done in the ring R (grevlex)
contractToPolynomialRing = method(Options => {Verbosity => 0})
contractToPolynomialRing(Ideal) := opts -> I -> (
    -- assumes: I is in a ring k(basevars)[fibervars] created with makeFiberRings
    -- returns the intersection of I with k[fibervars,basevars] (also created with makeFiberRing).
    --   note: numerator (and denominator) of element in ring I gives an element in k[fibervars,basevars]
    if not instance(coefficientRing ring I, FractionField) then return I; -- in this case, we are already contracted!
    newI := I_*/numerator//ideal//trim;
    S := ring newI;
    denoms := I_*/denominator;
    denomList := unique flatten for d in denoms list (factors d)/last;
    if opts.Verbosity > 0 then << "denoms = " << denoms << " and denomList = " << denomList << endl;
    Isat := S.cache#"StoR" newI;
    F := S.cache#"StoR" (product denomList);
    Isat = mySat(Isat, F);
    --for f in denomList do Isat = saturate(Isat, S.cache#"StoR" f);
    S.cache#"RtoS" Isat)

-- getGB is used for finding a lower bound on the codim of the ideal I
-- The idea is that sometimes the GB computation is too huge, and we
-- don't want to undertake that.  But, if it is there, we want to take
-- advantage of it.  It could even be a partial Groebner basis.
-- codimLowerBound below uses whatever lead terms we can find in the ideal
-- to get some lower bound on the codimension.
getGB = method()
getGB Ideal := I -> (
    pos := select(1, keys I.generators.cache, k -> instance(k, GroebnerBasisOptions));
    if #pos === 0 then null else I.generators.cache#(first pos))

-- this function returns a list of the unique factors (of positive degree)
--  occurring in polyList, made monic.
monicUniqueFactors = polyList -> (
    polyList1 := polyList/factors//flatten;
    polyList2 := select(polyList1, g -> #g > 0);
    polyList2 / last // unique)

---------------------------------------------
--- General AnnotatedIdeal commands ---------
---------------------------------------------

gb   AnnotatedIdeal := opts -> I ->  I.Ideal = ideal gens gb(I.Ideal, opts)
trim AnnotatedIdeal := opts -> I -> (I.Ideal = trim I.Ideal; I.Trim = true; I)

-- The associated ideal to an annotated ideal I is
-- defined at the top of this file.
-- TODO Notes (23 April 2013):
--  (1) Possibly split the linears into two groups, and add the ones with denom=1
--      after the saturation is done.
--  (2) Should we be saturating with the I.Inverted \ I.NonzeroDivisors polynomials?
--      Answer should be yes: but if we know the ideal is prime, then
--      we think we can avoid this.  BUT: we need to be very precise about
--      this logic.

-- TODO: should the cache symbol just be Ideal?
ideal AnnotatedIdeal := Ideal => (cacheValue "CachedIdeal") (I -> (
    --F := product unique join(I.Linears / (x -> x#1), I.Inverted);
    I2 := if not I.?IndependentSet then I.Ideal else (
	S := I.IndependentSet#1;
	phi := S.cache#"StoR";
	phi contractToPolynomialRing ideal I.LexGBOverBase);
    for linear in reverse I.Linears do (
	I2 = I2 + ideal last linear;
	if linear#1 == 1 then continue;
	facs := factors linear#1;
	if #facs != 0 then I2 = mySat(I2, facs / last // product);
	I2 = trim I2);
    I2))

-*
-- TODO: is this still needed?
ideal AnnotatedIdeal := (I) -> (
    --F := product unique join(I.Linears / (x -> x#1),I.Inverted);
    if not I#?"CachedIdeal" then I#"CachedIdeal" = (
       F := product unique (I.Linears / (x -> x#1));
       I1 := ideal(I.Linears/last);
       I2 := if I.?IndependentSet then (
               S := (I.IndependentSet)#1;
               phi := S.cache#"StoR";
               phi contractToPolynomialRing ideal I.LexGBOverBase
             )
             else
               I.Ideal;
       I3 := if numgens I1 === 0 then I2 else if numgens I2 === 0 then I1 else I1+I2;
       --if F == 1 then I3 else saturate(I3, F)
       if F == 1 then I3 else mySat(I3, F)
    );
    I#"CachedIdeal"
)
*-

isSubset(Ideal, AnnotatedIdeal) := (I, J) -> isSubset(I, ideal J) -- naive attempt first...

-- Note that if I.IndependentSet is set, then I.Ideal is not the entire ideal.
-- However in this case, I.isPrime will (TODO: check this!) have previously
-- been set to "UNKNOWN", or maybe to "YES" during the IndependentSet or
-- SplitTower computations.
isPrime AnnotatedIdeal := {} >> o -> (I) -> (
    if I.?IndependentSet and not I.?isPrime
      then error "Our isPrime logic is wrong in this case.";
    if not I.?isPrime or I.isPrime === "UNKNOWN" then (
        I.isPrime = if numgens I.Ideal == 0 then "YES" else
                    if I.?Factorization and numgens I.Ideal == 1 then "YES" else
                    "UNKNOWN";
       );
    I.isPrime
    )

codim AnnotatedIdeal := options(codim,Ideal) >> opts -> (I) -> (
     if I.?LexGBOverBase then (
         -- this codim is correct in all cases
         S := ring I.LexGBOverBase#0; -- should be of the form kk(indepvars)[fibervars]
         # I.Linears + numgens S
         )
     else (
         -- this codim may be only a codimLowerBound if an element in the
         -- inverted list is in all the minimal primes of top dimension
         if not I.?isPrime or I.isPrime =!= "YES" then
            << "Warning : codim AnnotatedIdeal called on ideal not known to be prime." << endl;
         # I.Linears + codim(I.Ideal)
     )
     )

codimLowerBound = method()
codimLowerBound AnnotatedIdeal := I -> (
    if I.?LexGBOverBase then (
	-- again, in this case, this is the actual codimension.
	S := ring I.LexGBOverBase#0; -- should be of the form kk(indepvars)[fibervars]
	# I.Linears + numgens S)
    else (
	GB := getGB I.Ideal;
	if GB =!= null
	then # I.Linears + codim(monomialIdeal leadTerm GB)
	else if numgens I.Ideal === 1 and I.Ideal_0 != 0
	then # I.Linears + 1
	else # I.Linears + codim(monomialIdeal gens I.Ideal))
    )

--- this is so that we can add in generators to I and keep track of
--- how the annotation changes
adjoinElement = method()
adjoinElement(AnnotatedIdeal, RingElement) := (I, f) -> (
    J := ideal compress ((gens I.Ideal) % f);
    J = if J == 0 then ideal f else (ideal f) + J;
    annotatedIdeal(J, -- is a trim necessary/desirable here?
	I.Linears,    -- 'linear' generators
	{},           -- clear out nonzerodivisor list
	unique join(I.NonzeroDivisors, I.Inverted) -- move nonzerodivisors to inverted list
	)
    )

--------------------------------------------------------------------
----- Tests section
--------------------------------------------------------------------

TEST ///
   debug needsPackage "MinimalPrimes"
   R = QQ[a,b,c]
   polyList = {2*a^2,b^2,c^2,a^3,1_R,8*b^3-c^3}
   muf = monicUniqueFactors polyList
   assert(set muf === set {a, b, c, b-(1/2)*c, b^2+(1/2)*b*c+(1/4)*c^2})
///

TEST ///
  debug needsPackage "MinimalPrimes"
  R = QQ[b,s,t,u,v,w,x,y,z]
  I = ideal"su - bv, tv - sw, vx - uy, wy - vz"
  J1 = annotatedIdeal(trim ideal 0_R,{},{},{})
  assert(ideal J1 == 0)
  assert(ring J1 === R)
  assert(codim J1 == 0)
  J2Linears = {(z, v, - w*y + v*z), (y, u, - v*x + u*y), (w, s, - t*v + s*w), (v, b, - s*u + b*v)}
  J2 = annotatedIdeal(trim ideal 0_R, J2Linears,{v, u, s, b},{})
  F = product unique (J2Linears / (x -> x#1))
  assert(ideal J2 == saturate(ideal (J2Linears / last), F))
  assert(ring J2 === R)
  assert(codim J2 == 4)
  J2 = trim J2
  assert(J2.Trim)
  J3 = annotatedIdeal(trim ideal 1_R,{},{},{})
  assert(ideal J3 == 1)
  assert(ring J3 === R)
  assert(codim J3 == infinity)
  --- TODO: An example with IndependentSets/SplitTower
///
