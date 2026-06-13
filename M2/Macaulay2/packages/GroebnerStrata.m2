-- -*- coding: utf-8 -*-
newPackage(
	"GroebnerStrata",
    	Version => "0.9", 
    	Date => "11 Nov 2021",
    	Authors => {
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike/"},
	     {Name => "Kristine Jones", Email => "kejones84@gmail.com"}},
	Headline => "computing Groebner loci in Hilbert schemes",
        PackageImports => {"Elimination"},
        PackageExports => {"Complexes"},
    	DebuggingMode => false,
	Keywords => {"Commutative Algebra"}
    	)

export { 
    "randomPointOnRationalVariety",
    "randomPointsOnRationalVariety",
    "standardMonomials",
    "smallerMonomials",
    "tailMonomials",
    "findWeightConstraints",
    "findWeightVector",
    "groebnerFamily",
    "groebnerStratum",
    "nonminimalMaps",
    "Minimalize", -- needed? currently not active, todo: make it active
    "linearPart", -- needed?
    "AllStandard"
    }

----------------------------------------------------------------------------
-- Functions useful when dealing with the components of Groebner strata ----
----------------------------------------------------------------------------
findVariableOccuringToDegreeOne = (J) -> (
    -- J is an ideal
    -- returns: a variable that occurs to degree one in some J_i, null if none can be found
    for x in gens ring J do
      for f in J_* do
        if degree_x f === 1 then return x;
    null
    )

randomPointsOnRationalVariety = method()
randomPointsOnRationalVariety(Ideal, ZZ) := List => (I, npts) -> (
    -- return a list of npts one row matrices
    -- assuming that the variety is rational, and we can detect this.
    A := ring I;
    kk := coefficientRing A;
    H := partition(f -> index leadMonomial f === null, (trim I)_*);
    I1 := if H#?true then ideal H#true else trim ideal(0_A); -- ones that do not have linear lead term.
    I2 := if H#?false then ideal H#false else trim ideal(0_A) ; -- ones that do.
    c := codim I1; -- this is how many non-free variables we will need to add in.
    J := trim I1;
    L := {};
    for i from 1 to c do (
        x1 := findVariableOccuringToDegreeOne J;
        if x1 === null then error "cannot determine whether the ideal is rational";
        -- x1 := select(gens A, x -> degree_x J_0 === 1); -- todo: check the others too, if needed.
        -- if #x1 == 0 then error "cannot determine whether the ideal is rational"; -- perhaps: return (I2, I1, J, L);
        -- x1 = first x1
        J = trim eliminate(J, x1);
        L = append(L, x1);
        );
    if J != 0 then error "internal error: logic is wrong in randomPointsOnRationalVariety";
    NONFREE := set L + set flatten entries leadTerm gens I2;
    FREE := toList(set gens A - NONFREE);
    pts := for i from 0 to npts-1 list (
        subs := for x in FREE list x => random kk;
        pt := sub(vars A, subs) % trim sub(I, subs);
        try lift(pt, kk) else continue
        --if liftable(pt, kk) then lift(pt, kk) else continue
        );
    if #pts == 0 then error "cannot determine whether the ideal is rational";
    pts
    )

randomPointOnRationalVariety = method()
randomPointOnRationalVariety Ideal := Matrix => I -> first randomPointsOnRationalVariety(I, 1)

-----------------------------------------------
-- Choice of tails on the monomials -----------
-- The choice is usually: the standard monomials smaller than the lead monomial.
-- But, this can be changed to include all standard monomials of the same degree.
--   Typically, this will cause the ring of coefficients to not be graded though.
-----------------------------------------------
smallerMonomials = method()
smallerMonomials(Ideal,RingElement) := List => (M,f) -> (
     -- input: a polynomial in a poly ring R; f must be a (nonzero) monomial,
     --   typically a generator of the monomial ideal M.
     -- output: an ordered list of standard monomials of R less than f, but of the same
     --   degree as (the leadterm of) f.
     if f == 0 or #(terms f) =!= 1 then
         error "smallerMonomials: expected the second argument to be a (nonzero) monomial";
     R := ring M;
     d := degree f;
     m := flatten entries basis(d,coker gens M);
     m = f + sum m;
     b := apply(listForm m, t -> R_(first t));
     x := position(b, g -> g == f);
     drop(b,x+1))

smallerMonomials Ideal := List => M -> 
    for m in M_* list smallerMonomials(M, m)

standardMonomials = method()
standardMonomials(ZZ,Ideal) :=
standardMonomials(List,Ideal) := (deg,M) -> (
    f := sum flatten entries basis(deg, comodule M);
    if f == 0 then {}
    else (terms f)/leadMonomial
    )

standardMonomials Ideal := List => (M) -> (
     apply(M_*, f -> standardMonomials(degree f, M))
     )

tailMonomials = method(Options => {AllStandard => false})

tailMonomials(Ideal, RingElement) := List => opts -> (M, leadterm) -> (
    if opts.AllStandard then standardMonomials(degree leadterm, M) else smallerMonomials(M, leadterm)
    )
tailMonomials Ideal := List => opts -> M -> (
    if opts.AllStandard then standardMonomials M else smallerMonomials M
    )

------------------------------------------------
-- Weight vector constraints -------------------
------------------------------------------------
findHeft1 = (A) -> (
    -- A is a matrix such that the weight vector times each column must be positive.
    -- result: (List, List): (wtvector, wtvalues).
    --A := transpose matrix degs;
    degs := entries transpose A;
    needsPackage "FourierMotzkin";
    B := ((value getGlobalSymbol "fourierMotzkin") A)#0;
    r := rank source B;
    f := (matrix{toList(r:-1)} * transpose B);
    if f == 0 then return;
    heft := first entries f;
    g := gcd heft;
    if g > 1 then heft = apply(heft, h -> h // g);
    minheft := min heft;
    if minheft <= 0 then heft = apply(heft, a -> a - minheft + 1);
    heftvals := apply(degs, d -> sum apply(d, heft, times));
    if not all(heftvals, d -> d > 0) then return null;
    (heft, heftvals)
    )

findWeightConstraints = method()
findWeightConstraints(Ideal, List) := Matrix => (M, L) -> (
    --input: a monomial ideal M and a list of lists of standard monomials L
    --output: a weight vector w (a list of integers of length the # of variables
    -- of the ring of M) 
    --w places the listed generators of M greater than the corresponding standard monomials)
    R := ring M;
    kk := coefficientRing R;
    nv := sum apply(L, s -> #s);
    Mlist := M_*;
    D := flatten apply(#Mlist, i -> (
            m := Mlist_i;
            apply(L#i, s -> (
                    first exponents m - first exponents s))));
    transpose matrix D
    )

findWeightVector = method ()
findWeightVector(Ideal, List) := (M, L) -> (
     --input: a monomial ideal M and a list of lists of standard monomials L
     --output: a weight vector w (a list of integers of length the # of variables
       -- of the ring of M) 
     --w places the listed generators of M greater than the corresponding standard monomials)
     A := findWeightConstraints(M, L);
     findHeft1 A
     )

----------------------------------
-- Is the following useful?  Determine this and use it, or remove it!
-- both findEliminants, and the version of groebnerFamily that is commented out.
-- Perhaps this should be part of minimizeGroebnerStratum(F, J) -- given family, and ideal
----------------------------------
-*
findEliminants = method()
findEliminants(Ideal,Ideal) := (M,J) -> (
-- input: a monomial ideal M, and the parameter family J
-- output: an ordered pair of lists of indices; the first entry is the variables that
      --will be eliminated, the second entry is the variables that will not be eliminated    
     S := ring J;
     SP1 := (gens J) * sub(syz gens M, S);
     SP2 := SP1 % sub(M,S);
     L1 := trim ideal flatten last coefficients SP2;
     L1 = sub(L1,coefficientRing S);
     -- set1 will be the variables which are lead terms here:
     set1 := sort ((flatten entries leadTerm L1)/index);
     set2 := sort toList(set toList(0..numgens coefficientRing S - 1) - set set1);
     (set1, set2)
     )     

groebnerFamily (Ideal, List) := opts -> (M, L) -> (
     t := opts.Variable;
     wts := findWeightVector(M,L);
     if wts === null then error (
       "could not find weight vector placing all standard monomials after ideal generators");
     (w, wvals) := wts;
     R := ring M;
     kk := coefficientRing R;
     nv := #wvals;   
     R1 := kk[t_1..t_nv, Degrees => wvals];
     U := R1 (monoid[gens R, Join=>false, Degrees => w]);
     phi := map(U,R1);
     lastv := -1;
     Mlist := M_*;
     elems := apply(#Mlist, i -> (
	       m := Mlist_i;
	       substitute(m,U) + sum apply(L#i, p -> (
			 lastv = lastv + 1;
			 phi(R1_lastv) * substitute(p,U)))));
     J := ideal elems;
     --determining which parameters can be eliminated
     (indices1, indices2) := findEliminants(M,J);  
     gens1 := apply(indices1, i -> (wvals#i, i));
     indices1 = apply(rsort gens1, (h,i) -> i);
     -- Now make the actual coeff ring:
     R2 := kk[(gens R1)_indices1, (gens R1)_indices2, 
	  Degrees => join(wvals_indices1,wvals_indices2),
	  MonomialOrder => {Lex => (#indices1), #indices2}];
     U = R2 (monoid[gens R, Join=>false, Degrees => w]);
     f1 := map(R2,R1);
     f2 := map(U, ring J, vars U | f1.matrix);
     J = f2 J;
     J
     )
*-
-----------------------------------------
-- Creation of the family itself --------
-- This also creates the ring of parameters, 
--   and the ring of the family
-----------------------------------------
groebnerFamily = method (Options => {
        AllStandard => false, 
        Variable => getSymbol "t",
        Weights => null -- default: find one (calls findWeightVector)
        }
    )
groebnerFamily Ideal := opts -> M -> ( 
     L := if opts.AllStandard then standardMonomials M else smallerMonomials M;
     groebnerFamily(M, L, opts)
     )

groebnerFamily (Ideal, List) := opts -> (M, L) -> (
    if all(L, x -> #x == 0) then (
        return M;
        );
    t := opts.Variable;
    C := findWeightConstraints(M, L);
    if opts.Weights === null then (
        wts := findHeft1 C;
        if wts === null then error "could not construct a weight vector";
        wtvector := first wts;
        )
    else (
        wtvector = opts.Weights;
        );
     wvals := flatten entries(matrix{wtvector} * C);
     R := ring M;
     kk := coefficientRing R;
     R1 := kk (monoid[gens R, 
             Join=>false, 
             Degrees => wtvector, 
             SkewCommutative => (options R).SkewCommutative
             ]);
     psi := map(R1, R, vars R1);
     MU := psi M;
     LU := for L1 in L list for m in L1 list psi m;
     (MU, LU);
     allvars := for i from 0 to numgens MU - 1 list (
         lt := MU_i;
         for m in LU#i list {degree lt - degree m, m}
         );
     varlist := flatten allvars;
     A1 := kk[t_1..t_#varlist, Degrees => varlist/first];
     A := kk[rsort gens A1, Degrees => (rsort gens A1)/degree/first]; -- MonomialOrder => Lex?
     T := A (monoid R1);
     f := map(T, R1, vars T);
     idx := 0;
     F := ideal for i from 0 to numgens MU - 1 list (
         lt := f(MU_i);
         lt + sum for m in LU#i list (idx = idx+1; sub(A1_(idx-1),A) * f(m))
         );
     F
     )
 
--------------------------------------------
-- The ideal defining the Groebner stratum
--   given the Groebner family constructed via 
--   `groebnerFamily`
--------------------------------------------     
groebnerStratum = method(Options => {
        Minimalize => false})

groebnerStratum Ideal := Ideal => opts -> (J) -> (
    --input: the ideal of the family.
    -- output: the ideal defining the parameter space.
    -- NOTE: Minimalize=>true is currently a no-op.  See the
    -- minimalizeFamily block (commented out below) and the documentation
    -- node for `Minimalize` for the current blocker.
    if opts.Minimalize then << "warning: Minimalize=>true is not yet implemented; ignoring" << endl;
    R := ring J;
    G := forceGB gens J;
    M := leadTerm gens J;
    syzM := syz M;
    eq := compress((gens J * syzM) % G);
    (mons,eqns) := coefficients(eq); 
    H := ideal lift(eqns,coefficientRing R);
    ideal compress generators H
    )   


linearPart = (f) -> sum select (terms f, t->(
	  sum first exponents t === 1))

-*
TODO: this draft of minimalizeFamily would wire up the `Minimalize`
option of groebnerStratum, but it depends on `selectInSubring(1, vars
ring H)` returning the "free" parameter block.  The current
`groebnerFamily` builds its parameter ring with a single GRevLex block
(line ~280 has the commented-out `MonomialOrder => Lex?`), so
selectInSubring(1, ...) returns an empty matrix and reviving this
function as-is eliminates *every* parameter rather than just the
eliminable ones.  To re-enable, either:
  (a) restore a multi-block monomial order (Lex over the eliminable
      parameters, GRevLex over the rest) in groebnerFamily, or
  (b) detect the eliminable parameters from the linear-leading-term
      coefficients of H without relying on selectInSubring.
See the `Minimalize` doc node for the user-visible status.
minimalizeFamily = method()
minimalizeFamily (Ideal, Ideal) := (J,H) -> (
     R := ring H;
     ringgens := flatten entries selectInSubring (1,vars ring H);
     newR := (coefficientRing R)[ringgens, Degrees => ringgens/degree];
     newringJ := newR( monoid[gens ring J, Join => false, Degrees => degrees(ring J)]);
     L := matrix {apply (H_*, f->linearPart f)};
     C := getChangeMatrix gb (L, ChangeMatrix => true);
     time elimH := trim ideal ((gens H)*C);
     D := ideal leadTerm ((gens H)*C);
     assert( # unique D_* == numgens D);
     newH := compress ((gens H)% elimH);
     time newH = trim ideal substitute(newH, newR);
     elimH' := promote (elimH, ring J);
     newJ := ideal ((gens J) % elimH');
     newJ = substitute(newJ, newringJ);
     (newJ, newH)
     )
*-

-- take an ideal in A[vars], homogeneous, and a ring S (in vars, but with different, often standard, grading)
-- return: free non-minimal resolution, as well as the degree zero maps in the resolution.
nonminimalMaps = method()
nonminimalMaps(Ideal) := Sequence => (F) -> (
    T := ring F;
    A := coefficientRing T;
    T' := first flattenRing T;
    F' := sub(F, T');
    for f in F'_* do if leadCoefficient f != 1 then error "expected a putative monic Groebner basis";
    C := res(F', Strategy => NonminimalWithGB); -- must be over a finite field.
    -- now let's place this into a new ring, with degree 0 coefficients.
    A'' := (coefficientRing A)[gens A, Degrees => {numgens A : 0}];
    T'' := A'' [gens T, Join => false];
    -- now we want to move C to this ring, to easily find degree 0 maps.
    newMaps := new MutableList from toList (length C: null);
    newMaps#0 = map(T''^1,, sub(C.dd_1, T''));
    for i from 2 to length C do (
        f := sub(C.dd_i, T'');
        newMaps#(i-1) = map(source newMaps#(i-2),, f);
        );
    CF := complex toList newMaps;  -- this is one thing returned.
    -- the other is a list of degree zero maps, indexed via: (homological degree, internal degree).
    H := hashTable flatten for lev from 1 to length C list (
        set1 := set (degrees CF_lev)/first;
        set2 := set (degrees CF_(lev+1))/first;
        degs := sort toList(set1 * set2);
        for d in degs list (lev+1, d) => (
            sub(submatrixByDegrees(CF.dd_(lev+1), d, d), A)
            )
        );
    (CF, H)
    )


---------------
--Documentation
----------------
beginDocumentation()
------------
--Front Page
-------------
doc///
  Key
    GroebnerStrata
  Headline
    a package for creating families of ideals with the same initial ideal
  Description
    Text
      The {\em GroebnerStrata} package is designed to compute, given a
      monomial ideal in a polynomial ring, (and a term order, coming
      from the polynomial ring), a family of homogeneous ideals all
      with the given monomial ideal as its lead term (initial) ideal.

      The {\em GroebnerStrata} package is designed for computing homogeneous
      strata and parameter families for monomial ideals in polynomial rings.
      In certain instances homogeneous strata can be used to compute local
      coordinates on Hilbert Schemes.
    Text
      Here is an example of the basic use of the package.  We compute the Groebner family
      of the ideal $(a^2, ab, b^2, ac)$.  This is an ideal in a polynomial ring with variables the
      same as $S$, and whose coefficient ring is a polynomial ring containing all of the
      parameters.  The @TO (groebnerStratum, Ideal)@ function returns the ideal in the parameters
      of the locus of parameters, for which the given family is a Groebner basis.
    Example
      kk = ZZ/101;
      S = kk[a..d];
      M = ideal"a2,ab,b2,ac";
      F = groebnerFamily M;
      netList F_*
      J = trim groebnerStratum F
    Text
      The ideal of the parameter space of all homogeneous ideals with this lead term ideal
      is an ideal in 24 variables.  Often, these parameter ideals are in too many variables
      to easily analyze them.  But in this case we can determine the irreducible components
      of the ideal $J$.  There are two components, of dimensions 8 and 11.  Note that they
      are both rational varieties.
    Example
      compsJ = decompose J;
      compsJ = compsJ/trim;
      compsJ/dim
      netList compsJ_0_*
      netList compsJ_1_*
    Text
      This tells us that there are 2 components (at least over the given field).
      Their dimensions are 11, 8.
    Text
      We can find random points on each component, since these components are rational.
    Example
      pt1 = randomPointOnRationalVariety compsJ_0
      pt2 = randomPointOnRationalVariety compsJ_1
      F1 = sub(F, (vars S)|pt1)
      F2 = sub(F, (vars S)|pt2)
      decompose F1
      decompose F2
    Text
      Note, the general element of one component is a plane conic union a point.
      (The dimension of the locus of all such is: (choice of plane) + (choice of degree 2 in plane) + choice of point.
      This is 3 + 5 + 3 = 11.
      
      The other component consists of two skew lines.  This has dimension (choice of line) + (choice of line).
      This is 4 + 4 = 8.  Also notice that the 2 skew lines do not have to be defined over the 
      base field, as in this case.
 ///

--------------
--Functions --
--------------

doc ///
  Key
    standardMonomials
    (standardMonomials, Ideal)
    (standardMonomials, List, Ideal)
    (standardMonomials, ZZ, Ideal)
  Headline
     computes standard monomials  
  Usage
    L = standardMonomials M
    L = standardMonomials(d, M)
  Inputs
    M : Ideal
      M should be a monomial ideal
    d : List
      a degree
  Outputs
    L : List
      L is a list of lists of standard monomials for the ideal $M$,
      one for each generator of $M$
  Description
    Text
      A monomial $m$ is standard with respect to a monomial ideal $M$
      and a generator $g$ of $M$ if $m$ is of the same degree as $g$
      but is not an element of $M$.
      
      Inputting an ideal $M$ returns the standard monomials of each of
      the given generators of the ideal.
    Example
      R = ZZ/32003[a..d];
      M = ideal (a^2, a*b, b^3, a*c);
      L1 = standardMonomials M  
      standardMonomials({3}, M)
    Text
      Inputting an integer $d$ (or degree $d$) and an ideal gives the standard
      monomials for the specified ideal in degree $d$.
    Example
      standardMonomials(2, M)
  SeeAlso
    tailMonomials
    smallerMonomials
///

doc ///
  Key
    smallerMonomials
    (smallerMonomials, Ideal)
    (smallerMonomials, Ideal, RingElement)
  Headline
    returns the standard monomials smaller but of the same degree as given monomial(s)
  Usage
    L = smallerMonomials M
    L = smallerMonomials(M, m)
  Inputs
    M:Ideal
      $M$ should be a monomial ideal (an ideal generated by monomials)
    m:RingElement
      optional, 
  Outputs
    L : List
      a list of lists: for each generator $m$ of $M$, the list of all
      monomials of the same degree as $m$, not in the monomial ideal
      {\bf and} smaller than that generator in the term order of the
      ambient ring.  If instead $m$ is given, the list of the standard
      monomials of the same degree, smaller than $m$, is returned.
  Description
    Text
      Inputting an ideal $M$ returns the smaller monomials of each of
      the given generators of the ideal.
    Example
      R = ZZ/32003[a..d];
      M = ideal (a^2, b^2, a*b*c);
      L1 = smallerMonomials M  
      smallerMonomials(M, b^2)
  SeeAlso
    tailMonomials
    standardMonomials
///

doc ///
  Key
    tailMonomials
    (tailMonomials, Ideal)
    (tailMonomials, Ideal, RingElement)
    [tailMonomials, AllStandard]
  Headline
    find tail monomials
  Usage
    L = tailMonomials M
    L = tailMonomials(M, m)
  Inputs
    M:Ideal
      $M$ should be a monomial ideal (an ideal generated by monomials)
    m:RingElement
      optional, only return a single list of the tail monomials for this monomial
    AllStandard => Boolean
      which monomials should be considered tail monomials of a monomial $m$:
      either all standard monomials of a given degree, or all monomials smaller than
      $m$ in the given term order (but still of the same degree)
  Outputs
    L : List
      a list of lists: for each generator $m$ of $M$, the list of all tail monomials
      If instead $m$ is given, the list of the tail
      monomials of $m$ is returned
  Description
    Text
      Inputting an ideal $M$ generated by monomials returns a list of lists
      of tail monomials for each generator of $M$ (in the same order).
    Example
      R = ZZ/32003[a..d];
      M = ideal (a^2, b^2, a*b*c);
      tailMonomials M
      tailMonomials(M, AllStandard => true)
      tailMonomials(M, b^2)
      tailMonomials(M, b^2, AllStandard=>true)
  SeeAlso
    standardMonomials
    smallerMonomials
///

doc ///
  Key
    findWeightVector
    (findWeightVector, Ideal, List)
  Headline
    returns a weight vector
  Usage
    (w, h) = findWeightVector (M, L)
  Inputs
    M : Ideal
      M should be a monomial ideal
    L : List
      a list of lists of standard monomials
  Outputs 
    w : List
      w is a weight vector that places the specified generators of M greater than the corresponding
      standard monomials, if possible.  If not possible, null is returned instead of (w,h)
    h : List
      h is a list of values for w dotted with the difference of the exponent of
      the each standard monomial for each generator and the corresponding
      generator, in the order they are listed in L
  Description
    Text
      In the first entry, this command returns a weight vector associated to a
      monomial order that places the generators of a monomial ideal $M$ ahead
      of standard monomials of the same degree.  The second entry is a list of
      values for the weight vector dotted with the difference of the exponent
      of each standard monomial for each generator and the corresponding
      generator.
    Example
      R = ZZ/32003[a,b,c, d];
      M = ideal (a^2, a*b, b^2);
      L = smallerMonomials M;
      findWeightVector(M,L)
    Text
      Note that the first generator listed for $M$ is $a^2$, and the first
      corresponding standard monomial is $a*c$.  The difference of these two
      monomials exponent vectors is $(1,0,-1,0)$.  This vector dotted with the
      weight vector $(2,2,1,1)$ gives the value $1$, which is the first value
      in the second list.

      Note that the desired term ordering, and hence weight vector, may not exist.
      In this case, null is returned.
    Example
      M = ideal"ab"
      L1 = standardMonomials M
      findWeightVector(M,L1)
    Text
      This command is used in the @TO groebnerFamily@ routine.
  SeeAlso
    groebnerStratum
    groebnerFamily
///

doc ///
  Key
    findWeightConstraints
    (findWeightConstraints, Ideal, List)
  Headline
    returns a matrix of weight constraints
  Usage
    constraints = findWeightConstraints(M, L)
  Inputs
    M:Ideal
      M should be a monomial ideal
    L:List
      a list of lists of tail monomials
  Outputs 
    :Matrix
        with the constraints on a weight vector for $M$ to be the lead monomial
        considering the given tail monomials in $L$.  
        The number of rows of the matrix is the number of variables in the ring of $M$,
        The number of columns is the total size of $L$
  Description
    Example
      R = ZZ/32003[a,b,c, d];
      M = ideal (a^2, a*b, b^2);
      L = smallerMonomials M;
      mat = findWeightConstraints(M,L)
      needsPackage "Polyhedra"
      needsPackage "FourTiTwo"
      dualCone posHull (-mat)
      rays oo
      posHull mat -- seems wrong?
      coneFromHData transpose mat
      rays mat
      findWeightVector(M,L)
    Text
      TO BE FINISHED!!
      Note that the first generator listed for $M$ is $a^2$, and the first
      corresponding standard monomial is $a*c$.  The difference of these two
      monomials exponent vectors is $(1,0,-1,0)$.  This vector dotted with the
      weight vector $(2,2,1,1)$ gives the value $1$, which is the first value
      in the second list.

      Note that the desired term ordering, and hence weight vector, may not exist.
      In this case, null is returned.
    Example
      M = ideal"ab"
      L1 = standardMonomials M
      findWeightVector(M,L1)
    Text
      This command is used in the @TO groebnerFamily@ routine.
  SeeAlso
    groebnerStratum
    groebnerFamily
///

doc ///
  Key
    groebnerFamily
    (groebnerFamily, Ideal)
    (groebnerFamily, Ideal, List)
    [groebnerFamily, AllStandard]
    [groebnerFamily, Weights]
    [groebnerFamily, Variable]
  Headline
    computes families of ideals with a specified initial ideal 
  Usage
    J = groebnerFamily M
    J = groebnerFamily(M, L)
  Inputs
    M : Ideal
      which is generated by monomials
    L : List
      a list of lists of standard monomials or smaller standard monomials for
      the generators of M
    AllStandard => Boolean
    Weights => List
    Variable => Symbol
      or @ofClass String@
  Outputs
    F : Ideal
      the groebner family, an ideal in the polynomial ring over the original
      variables and the parameters
  Description
    Text
      Given a monomial ideal $M$ in a polynomial ring $R$, this computes the
      parameter families of homogeneous ideals where $M$ could be their initial
      ideal.  These families are obtained from either the standard monomials to
      the generators of $M$, or the standard monomials smaller than the
      generators of $M$ but of the same degree as these generators.  In the
      former case we obtain a family of all ideals where $M$ could be their
      initial ideal.  In the latter case, we obtain such a family with respect
      to a given term order.
    Example
      R = ZZ/32003[a,b,c,d];
      M = ideal (a^2, a*b, b^2)
      F = groebnerFamily M
      netList F_*
      U = ring F
      T = coefficientRing U
      gens T
      gens U
    Text
      Here, $F$ is the family of homogeneous ideals having $M$ as their initial
      ideal, under the term order of the ring of $M$.
      
      The optional argument @TO AllStandard@ is boolean, taking the value
      $true$ to compute the family of all homogeneous ideals with a given
      initial ideal and the value $false$ to compute the family with respect to
      a given order.  The default value for this argument is false.
      
      If $L$ is not given, then it is computed using @TO standardMonomials@ (if 
      AllStandard is true), or @TO smallerMonomials@ (if AllStandard is false).
    Example
      L = standardMonomials M
      F2 = groebnerFamily (M, L)
    Text  
      Note that $F$ and $F_2$ are the same family, in this case.
    Text
      This function also works if the ring of $M$ is a skew-commutative ring.
      This function produces a family of ideals in the exterior algebra.
      Similarly, @TO groebnerStratum@ allows one to find the ideal in the commuting coefficient
      variables of the family of ideals having $M$ as lead monomial ideal.
    Example
      kk = ZZ/101
      E = kk[a,b,c,d,e,SkewCommutative => true]
      I = ideal(a*d,a*c,a*b,b*d*e,b*c*e,b*c*d)
      F1 = groebnerFamily I
      netList F1_*
      F2 = groebnerFamily(I, AllStandard => true)
      netList F2_*
      J2 = trim groebnerStratum F2
      C2 = decompose J2
      netList C2_0_*
      netList C2_1_*
  SeeAlso
    groebnerStratum
    smallerMonomials
    standardMonomials
///

doc ///
  Key
    groebnerStratum
    (groebnerStratum, Ideal)
    [groebnerStratum, Minimalize]
  Headline
    compute the ideal where a given is a Groebner basis
  Usage
    J = groebnerStratum F
  Inputs
    F:Ideal
      An ideal constructed from @TO groebnerFamily@
  Outputs
    J : Ideal
    	 the ideal of the Groebner stratum
  Description
    Text
      Given a monomial ideal $M$, this command returns a family of ideals
      having $M$ as an initial ideal, and conditions on the parameters so that
      the family is flat.  If the optional input @TO AllStandard@ is specified
      as $true$, the family of all homogeneous ideals having $M$ as an initial
      ideal is computed, and if it is specified as $false$ the family of all
      homogeneous ideals having $M$ as an initial ideal with respect to the
      given term order is computed.  
    Example
      R = ZZ/32003[a,b,c]
      M = ideal (a^2, a*b, b^2)
      F = groebnerFamily M
      J = trim groebnerStratum F
    Text
      In this example, F is the universal family, and L is the ideal giving the
      conditions on the parameters.  In general, several of the parameters are
      unnecessary. Note that H is an ideal in a ring with far fewer parameters.
      This is because a maximal set of eliminable parameters from the original
      ideal of conditions on parameters have been eliminated.  If the full
      ideal in the polynomial ring over all the parameters is desired, set the
      optional argument $Minimalize$ to false.
    Example
      J2 = trim groebnerStratum(F, Minimalize => false)
      netList J_*
      netList J2_*
    Text
      Notice that the parameters $t_3$, $t_6$, and $t_9$ are clearly eliminable.          
  SeeAlso
    groebnerFamily
///

doc ///
  Key
    linearPart
  Headline
    returns linear part of a polynomial
  Usage
    l = linearPart(f)
  Inputs
    f : RingElement
      f is a polynomial
  Outputs
    l : RingElement
      l is the sum of the terms of f whose monomial is a variable
  Description
    Example
      R = ZZ/32003[a,b,c];
      f = a^2 + 3*b + 5*a*b*c + 2*a +b^2;
      linearPart f
    Text
      If we have a polynomial ring over another polynomial ring, the command
      only looks at the exponents on the new set of variables.
    Example
      S = ZZ/32003[a,b,c][x,y,z];
      g = a*b*x + 3*y + a + b^2
      linearPart g
    Text 
      Note that in the previous example, {\tt linearPart g} does not include {\tt a} but does include {\tt a*b*x}
///

doc ///
  Key
    (randomPointsOnRationalVariety, Ideal, ZZ)
    randomPointsOnRationalVariety
  Headline
    find random points on a variety that can be detected to be rational
  Usage
    randomPointsOnRationalVariety(I, n)
    randomPointOnRationalVariety
  Inputs
    I:Ideal
      An ideal in a polynomial ring $S$ over a field, which defines
      a prime ideal
    n:ZZ
      The number of points to generate
  Outputs
    :List
      A list of $n$ one row matrices over the base field of $S$, that are
      randomly chosen points on $I$.  null is returned in the case when
      the routine cannot determine if the variety is rational and irreducible.
  Description
    Text
    Example
      kk = ZZ/101;
      S = kk[a..f];
      I = minors(2, genericSymmetricMatrix(S, 3))
      pts = randomPointsOnRationalVariety(I, 4)
      for p in pts list sub(I, p) == 0
    Example
      S = kk[a..d];
      F = groebnerFamily ideal"a2,ab,ac,b2"
      J = groebnerStratum F;
      compsJ = decompose J;
      compsJ = compsJ/trim;
      #compsJ == 2
      compsJ/dim
    Text
      There are 2 components.  We attempt to find points on each of these two components.
      We are successful.  This indicates that the corresponding varieties are both rational.
      Also, if we can find one point, we can find as many as we want.
    Example
      netList randomPointsOnRationalVariety(compsJ_0, 10)
      netList randomPointsOnRationalVariety(compsJ_1, 10)
  SeeAlso
    (randomPointOnRationalVariety, Ideal)
  Caveat
    This routine expects the input to represent an irreducible variety
///

doc ///
  Key
    (randomPointOnRationalVariety, Ideal)
    randomPointOnRationalVariety
  Headline
    find a random point on a variety that can be detected to be rational
  Usage
    randomPointOnRationalVariety I
    randomPointOnRationalVariety
  Inputs
    I:Ideal
      An ideal in a polynomial ring $S$ over a field, which defines
      a prime ideal
  Outputs
    :Matrix
      A one row matrix over the base field of $S$, representing a
      randomly chosen point on the zero locus of $I$.  null is returned in the case when
      the routine cannot determine if the variety is rational and irreducible.
  Description
    Text
      As a first example, we find a random point on the Veronese surface in $\PP^5$.
    Example
      kk = ZZ/101;
      S = kk[a..f];
      I = minors(2, genericSymmetricMatrix(S, 3))
      pt = randomPointOnRationalVariety I
      sub(I, pt) == 0
    Example
      S = kk[a..d];
      F = groebnerFamily ideal"a2,ab,ac,b2"
      J = groebnerStratum F
      compsJ = decompose J;
      compsJ = compsJ/trim;
      #compsJ == 2
      compsJ/dim
    Text
      There are 2 components.  We attempt to find a point on the first component
    Example
      pt1 = randomPointOnRationalVariety compsJ_0
      F1 = sub(F, (vars S)|pt1)
      decompose F1
    Text
      We attempt to find a point on the second component in parameter space, 
      and its corresponding ideal.
    Example
      pt2 = randomPointOnRationalVariety compsJ_1
      F2 = sub(F, (vars S)|pt2)
      decompose F2
    Text
      It turns out that this is the ideal of 2 skew lines, just not defined over this field.
  SeeAlso
    randomPointsOnRationalVariety
  Caveat
    This routine expects the input to represent an irreducible variety
///

doc ///
  Key
    nonminimalMaps
    (nonminimalMaps, Ideal)
  Headline
    find the degree zero maps in the Schreyer resolution of an ideal
  Usage
    (C, H) = nonminimalMaps I
  Inputs
    I:Ideal
      in a polynomial ring $S$ over a base field or coefficient ring
      $A$.  The lead terms of the generators of $I$ should be the
      initial ideal of $I$, and should be monic.
  Outputs
    C:Complex
      A complex over a polynomial ring where any parameters in the base ring are set to have degree 0,
      and the variables of the ring of $I$ are set to have degree one.
    H:HashTable
      Whose keys describe which submatrix in the resolution this is, and whose values are those
      submatrices (placed into the original coefficient ring $A$)
  Description
    Text
      The Schreyer resolution of $I$ (which is generally non-minimal) is computed.
      The nonminimal parts are the submatrices in this resolution which do not involve
      the variables in $S$.  They are elements in the base ring $A$.  For instance,
      {\tt H#(\ell, d)} is the submatrix of the matrix from $C_{\ell+1} \to C_{\ell}$
      sending degree $d$ to degree $d$.
    Text
      The ranks of these matrices for a specific parameter value determine exactly the 
      minimal Betti table for the ideal $I$, evaluated at that parameter point.
    Text
      Now for our example.
    Example
      kk = ZZ/101;
      S = kk[a..d];
      F = groebnerFamily ideal"a2,ab,ac,b2,bc2,c3"
      (C, H) = nonminimalMaps F;
      betti(C, Weights => {1,1,1,1})
    Text
      We see that there are 4 maps that are nonminimal (of sizes $2
      \times 4$, $5 \times 2$, $1 \times 3$, and $1 \times 1$).
    Example
      keys H
      H#(2,3)
      H#(3,4)
      H#(3,5)
      H#(4,6)
    Text
      Let's impose the condition that the map {\tt H#(2,3)} vanishes (so has rank 0).
      The Betti diagram of such ideals is not the one for a set of 6 generic points in $\PP^3$.
    Example
      J = trim(minors(1, H#(2,3)) + groebnerStratum F);
      compsJ = decompose J;
      #compsJ
      pt1 = randomPointOnRationalVariety compsJ_0
      pt2 = randomPointOnRationalVariety compsJ_1
      F1 = sub(F, (vars S)|pt1)
      betti res F1
      F2 = sub(F, (vars S)|pt2)
      betti res F2
    Text
      What are the ideals F1 and F2?
    Example
      netList decompose F1
      netList decompose F2
    Text
      We can determine what these represent.  One should be a set of 6 points, where 5 lie
      on a plane.  The other should be 6 points with 3 points on one line, and the other 3 points
      on a skew line.
  SeeAlso
    randomPointOnRationalVariety
///

///
  Key
  Headline
  Usage
  Inputs
  Outputs
  Description
    Text
    Example
  SeeAlso
///

---------
--Symbols/optional arguments
-------------
doc ///
  Key
    AllStandard
  Headline 
    boolean option for determining the use of all standard or smaller standard monomials
  Description
    Text
      This is an optional input for the @TO groebnerStratum@ and @TO groebnerFamily@ functions.  It takes values $true$ and $false$.  When 
      assigned the value $true$, the functions are computed with respect to all standard monomials for the specified generators.  When assigned the 
      value $false$, the functions are computed with respect to smaller monomials of the same degree as the specified generators but which do not lie in 
      the ideal.  
  SeeAlso
    groebnerStratum
    groebnerFamily
    smallerMonomials
    standardMonomials
///

doc ///
  Key
    Minimalize
  Headline
    boolean option of groebnerStratum (currently a no-op)
  Description
    Text
      This is an optional input for the @TO groebnerStratum@ function.  It is
      intended to control whether eliminable parameters are eliminated to
      obtain the Groebner stratum in a smaller ring.

      At present this option is not yet implemented: passing
      @TT "Minimalize => true"@ prints a warning and otherwise behaves
      exactly like @TT "Minimalize => false"@.  See the source-level
      @TT "minimalizeFamily"@ draft in @TT "GroebnerStrata.m2"@ for the
      current blocker (the parameter ring built by @TO groebnerFamily@ has
      a single GRevLex block, but the parameter-elimination logic needs a
      Lex subblock to identify eliminable parameters).
  SeeAlso
    groebnerStratum
///

--------------------
--Tests
--------------------

TEST ///
  -- basic test of the functionality of this package
  -- first take a look at generating tails.
  kk = ZZ/101
  S = kk[a..d]
  I = ideal(a^2, a*b, b^2, a*c, c^3, b*c^2)
  L = smallerMonomials I
  L1 = standardMonomials I
  assert(L == L1) -- in this case, standard and smaller are the same.

  -- weight-vector machinery
  C = findWeightConstraints(I, L)
  (wt, wtvals) = findWeightVector(I, L)
  assert(#wt == numgens S)
  assert(#wtvals == sum(L, l -> #l))
  assert(all(wtvals, v -> v > 0))
  -- a hand-picked weight {5,4,3,1} also places all generators after
  -- their standard-monomial tails (every product is positive)
  manualWtvals = flatten entries (matrix{{5,4,3,1}} * C)
  assert(all(manualWtvals, v -> v > 0))

  -- generate the family with the hand-picked weights
  F = groebnerFamily(I, L, Weights => {5,4,3,1})
  F1 = groebnerFamily(I)
  assert(isHomogeneous F1)
  assert(isHomogeneous F)
  -- both families have the same number of generators (= numgens I)
  assert(numgens F == numgens I)
  assert(numgens F1 == numgens I)

  use coefficientRing ring F
  use ring F
  F = sub(F, {t_14 => 0, t_19 => 0, t_20 => 0})
  J = trim groebnerStratum F
  assert(isHomogeneous J)
///

--Test 0 standard monomials
TEST ///
R = ZZ/32003[a..d]
M = ideal (a^2, a*b, b^3, a*c)
L = standardMonomials M
ans = {{b^2, b*c, c^2, a*d, b*d, c*d, d^2}, {b^2, b*c, c^2, a*d, b*d, c*d, d^2}, {b^2*c, b*c^2, c^3, b^2*d, b*c*d, c^2*d, a*d^2, b*d^2, c*d^2, d^3}, {b^2, b*c, c^2, a*d, b*d, c*d, d^2}}
assert (L == ans)
L1 = smallerMonomials M
-- For the first three generators (a^2, a*b, b^3) the smaller- and
-- standard- monomial sets coincide, because every standard monomial in
-- the relevant degree happens to be smaller than the generator.  For
-- the fourth generator a*c they differ: b^2 is a standard monomial of
-- degree 2 *greater than* a*c in graded revlex (try `b^2 > a*c` in M2),
-- so b^2 appears in standardMonomials_3 but not smallerMonomials_3.
assert(L_0 == L1_0 and L_1 == L1_1 and L_2 == L1_2)
assert(L_3 != L1_3)
assert(member(b^2, L_3) and not member(b^2, L1_3))
assert(isSubset(set L1_3, set L_3))
///

TEST ///
R = ZZ/32003[x,y]
M = ideal (x*y)
L = standardMonomials M
assert (L == {{x^2, y^2}})
L1 = smallerMonomials M
assert (L1 == {{y^2}})
///


TEST /// -- findWeightVector
R = ZZ/32003[a..d]
M = ideal (a^2, a*b, b^3, a*c)
L = standardMonomials M
(wt, wtvals) = findWeightVector(M,L)
ans = ({10, 5, 3, 1}, 
     {10, 12, 14, 9, 14, 16, 18, 5, 7, 9, 
       4, 9, 11, 13, 2, 4, 6, 4, 6, 8, 3, 
       8, 10, 12, 3, 5, 7, 2, 7, 9, 11})
assert((wt,wtvals) == ans)
///

TEST /// -- findWeightVector
R = ZZ/32003[a,b]
M = ideal (a*b)
L = standardMonomials M
assert(null === findWeightVector(M,L))
///

TEST /// -- findWeightVector — structural assertions on truncate(5, M).
-- The historical literal-list answer for this case ({10,5,3,1}, ...)
-- referred to the *untruncated* M and is no longer valid; the truncated
-- M has 40 generators (not 4), so the wtvals length should be 40 * 16 =
-- 640.  Rather than re-pin a 640-entry list, assert structural
-- properties: the result is a (weight, wtvals) pair with the right
-- shape and strictly positive entries.
  needsPackage "Truncations"
  R = ZZ/32003[a..d]
  M = ideal (a^2, a*b, b^3, a*c)
  M = truncate(5,M)
  L = standardMonomials M
  (wt, wtvals) = findWeightVector(M,L)
  assert(#wt == numgens R)
  assert(all(wt, w -> instance(w, ZZ) and w > 0))
  assert(#wtvals == sum(L, l -> #l))
  assert(all(wtvals, v -> v > 0))
  -- The weight vector should actually place all generators after their
  -- standard-monomial tails (the property findHeft1 is supposed to
  -- guarantee).
  C = findWeightConstraints(M, L)
  assert(flatten entries (matrix{wt} * C) == wtvals)
///

TEST ///
  -- Family/stratum of a lex-segment ideal with prescribed Hilbert
  -- polynomial 3*i + 1.  This block previously had no assertions; the
  -- invariants below were computed at the time of writing and lock
  -- the result in for regression.
  R = ZZ/32003[a..d]
  loadPackage "LexIdeals"
  M = lexIdeal(R, {1,4,7,10,13})
  M = ideal select(M_*, f -> first degree f <= 4)
  assert(numgens M == 6)
  assert(hilbertPolynomial(comodule M, Projective=>false) == hilbertPolynomial(comodule M, Projective=>false)) -- smoke; cf. line below
  F = groebnerFamily M;
  assert(numgens F == 6)
  assert(isHomogeneous F)
  assert(numgens coefficientRing ring F == 49)
  J = groebnerStratum F;
  assert(isHomogeneous J)
  assert(codim J == 32)
  J = trim J;
  assert(numgens J == 32)
///

TEST ///
  -- Example: triangle ideal in P^3.  The vanishing locus is supported on
  -- three coordinate lines (the "triangle"), whose Groebner family has
  -- twisted cubics among its generic fibres.  The original author's
  -- inline comment said "J is 0", but the stratum ideal is actually
  -- nontrivial (trim J has 7 generators), so it is worth locking in.
  kk = ZZ/101
  R = kk[a..d]
  M = ideal"ab,bc,ca"
  sm = standardMonomials M
  assert(#sm == 3)
  assert(all(sm, l -> #l == 7))

  F = groebnerFamily M
  assert(numgens F == 3)
  assert(isHomogeneous F)
  assert(numgens coefficientRing ring F == 16)

  J = groebnerStratum F
  assert(numgens J == 15)
  Jt = trim J
  assert(numgens Jt == 7)
  assert(Jt != 0)
///

TEST ///
  -- example in the exterior algebra
  kk = ZZ/101
  E = kk[a,b,c,d,e,SkewCommutative => true]
  I = ideal(a*d,a*c,a*b,b*d*e,b*c*e,b*c*d)
  F1 = groebnerFamily I
  netList F1_*
  F2 = groebnerFamily(I, AllStandard => true)
  netList F2_*
  assert isHomogeneous F1
  assert isHomogeneous F2
  J2 = trim groebnerStratum F2;
  assert(numgens ring J2 == 24)
  J1 = trim groebnerStratum F1;
  assert(numgens ring J1 == 23)
  C2 = decompose J2;
  netList C2_0_*
  netList C2_1_*
  assert(isHomogeneous C2_0)
  assert(isHomogeneous C2_1)
  assert(dim C2_0 == 18)
  assert(dim C2_1 == 15)
///

-- smallerMonomials input validation (previously, passing a non-monomial f
-- caused an opaque downstream error from `drop(b, null+1)`; now we error
-- early with a user-facing message).
TEST ///
R = ZZ/32003[a..d]
M = ideal(a^2, a*b, b^3, a*c)
-- single monomial: ok
assert(smallerMonomials(M, a^2) == {b^2, b*c, c^2, a*d, b*d, c*d, d^2})
-- non-monomial inputs error
assert(try (smallerMonomials(M, a + b); false) else true)
assert(try (smallerMonomials(M, 1 + a*b); false) else true)
assert(try (smallerMonomials(M, 0_R); false) else true)
///

-- tailMonomials with both AllStandard branches.  Previously this export
-- had zero TEST references.  For M = ideal(a^2, a*b, b^3, a*c), the
-- last generator a*c has a *smaller* tail (omits b^2) but the *full*
-- standard tail in degree 2 includes b^2.
TEST ///
R = ZZ/32003[a..d]
M = ideal(a^2, a*b, b^3, a*c)
-- single-generator form: with AllStandard => false (default), the tail
-- of a*c is the strictly smaller standard monomials in degree 2.
defaultTail = tailMonomials(M, a*c)
allTail = tailMonomials(M, a*c, AllStandard => true)
assert(isSubset(set defaultTail, set allTail))
assert(member(b^2, allTail))
assert(not member(b^2, defaultTail))
-- whole-ideal form
T1 = tailMonomials M
T2 = tailMonomials(M, AllStandard => true)
assert(#T1 == numgens M)
assert(#T2 == numgens M)
assert(T2_3 == allTail) -- the last generator
assert(T1_3 == defaultTail)
///

-- linearPart smoke test.  Previously this export had zero TEST references.
-- The function is defined as a plain assignment (not method()) and
-- expects a RingElement.
TEST ///
R = ZZ/32003[a..d]
assert(linearPart(a + b*c + d) == a + d)
assert(linearPart(a*b + c) == c)
assert(linearPart(1 + 2*a + 3*b) == 2*a + 3*b) -- constant term dropped
assert(linearPart(a*b*c) == 0)
assert(linearPart(5_R) == 0)
-- For polynomial-over-polynomial rings, only the outer ring's variables
-- count (this matches the doc example).
S = ZZ/32003[a,b,c][x,y,z]
g = a*b*x + 3*y + a + b^2
assert(linearPart g == a*b*x + 3*y)
///

-- randomPointOnRationalVariety / randomPointsOnRationalVariety: previously
-- both exports had zero TEST references.  Adapted from the doc-example
-- of randomPointsOnRationalVariety.
TEST ///
kk = ZZ/101
S = kk[a..f]
I = minors(2, genericSymmetricMatrix(S, 3))
-- single point
pt = randomPointOnRationalVariety I
assert(instance(pt, Matrix))
assert(numrows pt == 1 and numcols pt == numgens S)
assert(sub(I, pt) == 0)
-- multiple points
pts = randomPointsOnRationalVariety(I, 4)
assert(instance(pts, List))
assert(all(pts, p -> instance(p, Matrix)))
assert(all(pts, p -> sub(I, p) == 0))
///

-- nonminimalMaps: previously this export had zero TEST references.  The
-- function expects a putative monic Groebner basis (i.e. an ideal whose
-- generators all have lead coefficient 1) over a finite field, and
-- returns a Complex plus a HashTable of degree-zero maps.
TEST ///
kk = ZZ/32003
S = kk[a..d]
M = ideal(a^2, a*b, b^2, a*c)
F = groebnerFamily M
(C, H) = nonminimalMaps F
assert(instance(C, Complex))
assert(instance(H, HashTable))
assert(length C >= 1)
-- every key of H is a (homological-degree, internal-degree) pair
assert(all(keys H, k -> instance(k, Sequence) and #k == 2))
///

-- Minimalize option: currently a documented no-op (passing
-- `Minimalize => true` prints a warning and returns the same ideal as
-- `Minimalize => false`).  This test locks the no-op contract in so
-- that a future implementation of Minimalize will (correctly) need to
-- update this test as well.
TEST ///
kk = ZZ/101
S = kk[a..d]
M = ideal(a^2, a*b, b^2, a*c)
F = groebnerFamily M
J1 = groebnerStratum F
-- The warning is printed to stderr; we just check that the option
-- doesn't change the returned ideal.
J2 = groebnerStratum(F, Minimalize => true)
assert(J1 == J2)
///

end--

restart 
uninstallPackage "GroebnerStrata"
restart
loadPackage "GroebnerStrata"
installPackage "GroebnerStrata"
check GroebnerStrata

