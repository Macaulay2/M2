newPackage(
    "CylindricalAlgebraicDecomposition",
    Version => "1.0.4",
    Date => "2025/11/21",
    Headline => "(open) Cylindrical Algebraic Decompositions",
    Authors => {
    { Name => "Lee, C.",
      Email => "corin.lee@hotmail.co.uk",
      HomePage => "https://cel34-bath.github.io/index.html"},
    { Name => "del Rio, T.",
      Email => "teresodra@gmail.com",
      HomePage => "https://sites.google.com/view/tereso"},
    { Name => "Rahkooy, H.",
      Email => "rahkooy@gmail.com",
      HomePage => "https://people.maths.ox.ac.uk/rahkooy/"}
    },

    Keywords => {"Real Algebraic Geometry"},
    PackageExports => {"Elimination", "RealRoots"},
    AuxiliaryFiles => false,
    DebuggingMode => false
    )

export {
    "factorsInList",
    "evaluatePolynomials",
    "gmodsHeuristic",
    "lazardProjection",
    "projectionPhase",
    "samplePoints",
    "liftingPoint",
    "openCAD",
    "positivePoint",
    "findPositiveSolution",
    "hashify"
}

-* Code section *-

-- finds the support of a list of Polynomials
-- overloads original command to return the combined support of a list of polynomials.
support(List) := (L) -> (
    L1 := select(L, p -> not liftable(p, QQ));
    unique flatten (L1/support)
    )

-- find factors of all polynomials in a list, removing repetition
factorsInList = method()
factorsInList(List) := (L) -> (
    FL := flatten for p in L list (for g in factor p list g#0); -- returns the factors of each element of L and combines them into a single list.
    FL = select(FL, p -> not liftable(p, QQ)); -- removes any constants.
    FL = unique FL -- Reduces list to only the unique factors, removing multiplicity.
)

-- Evaluates the given RingElement or List of RingElements at a point given by a MutableHashTable.
evaluatePolynomials = method()
evaluatePolynomials(RingElement, MutableHashTable) := (p, alpha) -> (
    p = sub(p, apply(keys alpha, values alpha, (k, v) -> k => v)); -- substitute in all of the values for the variables specified in alpha.
    if liftable(p, QQ) then p = lift(p, QQ); -- if the output is a constant, lift it.
    p
    )
evaluatePolynomials(List, MutableHashTable) := (L, alpha) -> (
    apply(L, p -> evaluatePolynomials(p, alpha))
)

-- Finds the lead coefficient of a ring element with respect to a variable
leadCoefficient(RingElement, RingElement) := (p, v) -> (
  d := degree(v, p); -- obtain the highest degree of the specified variable
  contract(v^d, p) -- return the coefficient of the leading term.
)

-- Choose the next variable to project according to the heuristic gmods
gmodsHeuristic = method()
gmodsHeuristic(List, List) := (L, variables) -> (
    i := minPosition apply(variables, v -> sum(L, p -> degree(v, p))); -- find variable with lowest sum of variable degree in each polynomial.
    variables#i
)

-- Does one step of the projection phase
lazardProjection = method()
lazardProjection(List, RingElement) := (L, v) -> (
  -- if not(member(v, L)) then error "declared variable not present in list";
  L = factorsInList(L); -- ensure input polynomials are irreducible and pairwise relatively prime.
  L0 := {};
  P := partition(p -> member(v, support p), L);
  L = if P#?true then P#true else {};
  L0 = if P#?false then P#false else {}; --separate polynomials p not relying on v 
  -- these would create redundant calculations (resultants would be a power of p,
  -- discriminants and leading coefficient would be 0 and trailing coefficient would be p
  -- so we will just slot these back in later)
  -- "return the parts of each poly p in L that rely on v"
  L1 := flatten apply(L, p -> {leadCoefficient(p, v), p - v*contract(v, p), discriminant(p, v)}); -- leading coefficients, trailing coefficients, discriminants
  L2 := for p in subsets(L, 2) list resultant(p#0, p#1, v); -- resultants
  factorsInList join(L0, L1, L2) -- combine these into one list, as squarefree factors.
  )

-- Creates a full Lazard projection
projectionPhase = method()
projectionPhase(List) := (L) -> (
    L = factorsInList(L);
    S := {L};
    variables := support L; -- initial variables, the ones chosen already will be dropped
    ordering := {}; -- this will contain the variable ordering chosen
    if variables === {} then error "all polynomials are constants";
    while #variables > 1 do ( -- project recursively until you are left with univariate polynomials
      v := gmodsHeuristic(L, variables); -- identify variable to project away.
      L = lazardProjection(L, v); -- get projection in v
      variables = delete(v, variables); -- variable chosen is dropped
      S = prepend(L, S); -- projection polynomials are added to S.
      ordering = prepend(v, ordering); -- variable projected is added to ordering.
    );
    ordering = prepend(variables#0, ordering); -- the remaining variable is added to ordering.
    (S, ordering)
    )

-- Given a nonempty list of univariate polynomials, samplePoints prduces sample points for the cells (separating the roots)
samplePoints = method()
samplePoints(List) := (L) -> (
    if L=={} then error "expected non-empty list";
    if #(support L) != 1 then error "expected set of univariate polynomials";
    A := QQ(monoid[support L]);
    h := sub(product L, A);
    intervalSize := 1; 
    ourRoots := realRootIsolation(h, intervalSize); -- call RealRoots:-realRootIsolation (isolates real solutions of h in intervals of width at most 1)
    if #ourRoots==0 then (
        SP := {0_QQ}; -- if the polynomials have no roots, choose 0.
      )
      else (
    -- if two consecutive intervals have a shared start/end point that is a root then refine intervals:
      for i from 0 to #ourRoots-2 do (
        while (ourRoots#i#1)==(ourRoots#(i+1)#0) and sub(h, {(support h)#0=>ourRoots#i#1})==0 do (
          intervalSize = intervalSize/2;
          ourRoots = realRootIsolation(h, intervalSize);
        );
      );
      SP = for i from 0 to #ourRoots-2 list (ourRoots#i#1+ourRoots#(i+1)#0)/2; -- if there is only one root, this correctly returns an empty list.
      -- Add the beginning of the first interval and the end of the last interval to the list, but each of which -+1 in order to avoid them being a root:
      -- (putting all roots into QQ - get +-1 in ZZ if one root)
      SP = join({((ourRoots#0#0)-1)_QQ}, SP, {((ourRoots#-1#1)+1)_QQ});
    );
    SP
  )

-- Given the list of lists of polynomials that the projection returns creates a CAD in a tree-like hash structure
-- starting from the point p given. i is the level and could be deduced from p but it is sent to ease understanding
liftingPoint = method()
liftingPoint(List, List, MutableHashTable) := (S, ordering, alpha) -> (
    -- List (S) is a list of lists of polynomials, representing the projection polynomials at each level, starting in one variable, up to
    -- the initial list of polynomials in n variables.
    -- List (ordering) is the variable ordering followed in the projection (the first i variables are the variables at level i)    
    -- HashTable (alpha) is a point in i variables
    cell := new MutableHashTable;
    cell#"point" = alpha;
    i := #keys(alpha); -- number of variables that have been assigned
    -- we check if all the variables have been given a value already
    if i >= #S then cell else ( -- if so just return an empty MutableHashTable
        U := evaluatePolynomials(S#i, alpha); -- evaluating the polys in i+1 vars at point p (so U should be a set of univariate polynomials)
        cell#"polynomials" = U;
        -- Check in case U is not univariate.
        if #support(U) > 1 then error ("expected list of polynomials to have a single variable as support. The value of U is " | toString(U));
        v := ordering#i;
        for samplePoint in samplePoints(U) do (
            alphaNew := copy alpha;
            alphaNew#v = samplePoint;
            cell#samplePoint = liftingPoint(S, ordering, alphaNew);
        );
        cell
    )
)

-- project and lift the initial polynomials, performing a full open CAD.
openCAD = method()
openCAD(List) := (L) -> (
  (S, ordering) := projectionPhase(L);
  alpha := new MutableHashTable;
  liftingPoint(S, ordering, alpha)
)

-- Checks if there is a point in or above the given cell in which all the polynomials given in the list are strictly positive
positivePoint = method()
positivePoint(List, MutableHashTable) := (L, cell) -> (
    -- move down to bottom level, where all variables are evaluated.
    if #keys(cell#"point") < #support(L) then (
        for key in keys(cell) do(
            -- if the key is not "points" or "polynomials", call again 
            if not instance(key, String) then (
                result := positivePoint(L, cell#key);
                -- if the answer is a point (something different from null)
                if instance(result, HashTable) then return result;
            )
        );
        null --no point exists
    ) else (
        evaluations := evaluatePolynomials(L, cell#"point");
        evaluations = for e in evaluations list lift(e, QQ); -- elements in list were in R and not treated as numbers, this fixes that.
        if all(evaluations, e->(e>0)) then cell#"point" else null
    )
)

-- Checks if there is a point in which all the polynomials given in the list are strictly positive, and return it
findPositiveSolution = method()
findPositiveSolution(List) := (L) -> (
    result := positivePoint(L, openCAD(L));
    if instance(result, HashTable)
    then (true, hashify result)
    else (false, result)
)

-- Turns MutableHashTables into HashTables
hashify = method()
hashify(HashTable) := (H) -> (
   hashTable for KV in pairs H list KV#0 => hashify(KV#1)
    )
hashify(BasicList) := (H) -> (
    for x in H list hashify x
    )
hashify(Thing) := identity

-* Documentation section *-
beginDocumentation()

doc ///
Key
  CylindricalAlgebraicDecomposition
Headline
  Cylindrical Algebraic Decomposition
Description
  Text
    The CylindricalAlgebraicDecomposition package provides tools to compute open CADs of real algebraic sets.
    
    Cylindrical Algebraic Decomposition (CAD) is a fundamental tool in real algebraic geometry.  Given a family of polynomials in variables $x_1, \dots, x_n$, a CAD decomposes $\mathbb{R}^n$ into finitely many cylindrical cells such that each polynomial has constant sign on every cell. This is the basis for quantifier elimination over real closed fields and many applications in robotics, optimization, control theory, and symbolic computation.

    The package implements an open CAD using Lazard projection and recursive lifting.  The user-facing functions include:

    @TO "projectionPhase"@: Lazard projection
    @TO "liftingPoint"@: construct the open CAD above a given sample point
    @TO "openCAD"@: compute an open CAD of a list of polynomials
    @TO "positivePoint"@ and @TO "findPositiveSolution"@: locate sample points where all polynomials in a list are strictly positive

    We illustrate this package with a simple example. Consider the unit circle and cuspidal cubic:

    $\mathcal{F} = \{x^2 + y^2 - 1 = 0, x^3 - y^2 = 0\}$.

    The CAD of $\mathbb{R}^2$ with respect to $\mathcal{F}$ decomposes the plane into cells where the signs of the two polynomials are constant.

  Example
    R = QQ[x, y]
    f_1 = x^2 + y^2 - 1, f_2 = x^3 - y^2;
    F = {f_1, f_2}
    C0 = openCAD F
    hashify C0
    findPositiveSolution F
SeeAlso
  projectionPhase
  liftingPoint
  openCAD
  positivePoint
  findPositiveSolution
Subnodes
  findPositiveSolution
  hashify
///

doc ///
  Key
    (factorsInList, List)
    factorsInList
  Headline
    Full collection of factors.
  Usage
    factorsInList(L)
  Inputs
    L:List
     of polynomials in a ring.
  Outputs
    :List
      containing the factors of each polynomial, without multiplicity.
  Description
    Text
      This function returns all of the factors that appear in a list of RingElements, ignoring constants and multiplicity.
    Example
      R=QQ[x_1, x_2, x_3]
      p0=x_1*x_2, p1=x_1^2*x_2-x_1*x_3+x_3^3, p2=x_2^2*x_3+x_3;
      L={p0, p1, p2}
      factorsInList(L)
  SeeAlso
  Subnodes
///

doc ///
  Key
    evaluatePolynomials
    (evaluatePolynomials, RingElement, MutableHashTable)
    (evaluatePolynomials, List, MutableHashTable)
  Headline
    Evaluate polynomial(s) at point.
  Usage
    evaluatePolynomials(p, alpha)
    evaluatePolynomials(L, alpha)
  Inputs
    p:RingElement
      polynomial in a ring.
    L:List
      of polynomials in a ring.
    alpha:MutableHashTable
      point described using a mutable hash table where the keys are RingElements (variables in the ring) and the values are the associated sample point.
  Outputs
    :RingElement
      describing the polynomial evaluated at the sample point.
    :List
      of polynomials evaluated at the sample point.
  Description
    Text
      Given the polynomial (p) or list of polynomials (L) and sample point (alpha), evaluatePolynomials evaluates the 
      polynomial(s) at the sample point and returns the evaluated polynomial(s). 
      This is used in the lifting phase of the CAD, where a polynomial in k variables is evaluated at a 
      point $\alpha \in \mathbb{R}[x_1, \dots, x_{k-1}]$ to return a univariate polynomial in $\mathbb{R}[x_k]$.
    Example
      R=QQ[x_0, x_1, x_2, x_3]
      alpha = new MutableHashTable;
      alpha#(x_0) = 3, alpha#(x_1) = 4, alpha#(x_2) = 1;
      p0=x_1^2*x_0-2*x_3*x_2
      evaluatePolynomials(p0, alpha)
      alpha1 := copy alpha;
      alpha1#(x_3) = -2;
      evaluatePolynomials(p0, alpha1)
      p1=x_0*(x_1-1)*(x_2-2)*(x_3-3);
      L = {p0, p1}
      evaluatePolynomials(L, alpha)
      evaluatePolynomials(L, alpha1)
  SeeAlso
///

doc ///
  Key
    (leadCoefficient, RingElement, RingElement)
    leadCoefficient
  Headline
    Lead coefficient with respect to a variable.
  Usage
    leadCoefficient(p, v)
  Inputs
    p:RingElement
      a polynomial in the ring.
    v:RingElement
      a variable in the ring.
  Outputs
    :RingElement
      the leading coefficient of p with respect to the variable v.
  Description
    Text
      The leading coefficient of a RingElement with respect to a variable is returned.
    Example
      R=QQ[x_1, x_2, x_3]
      p=x_1^2*x_2-x_1*x_3+x_3^3
      leadCoefficient(p, x_1)
  SeeAlso
///

doc ///
  Key
    (gmodsHeuristic, List, List)
    gmodsHeuristic
  Headline
    Determine the next variable to project.
  Usage
    gmodsHeuristic(L, variables)
  Inputs
    L:List
      of polynomials in several variables.
    variables:List
      of variables in the polynomials provided.
  Outputs
    :RingElement
      the chosen variable to project.
  Description
    Text
      Given a list $L$ of polynomials in one or more variables, returns the variable with the lowest sum of degrees of the given polynomials. In case of tie, the 
      variable that appears earlier in support(L) is returned. This heuristic is motivated by the complexity analysis of CAD. Further information regarding this 
      heuristic can be found in @HREF "https://doi.org/10.1007/978-3-031-14788-3_17"@.
    Example
      R=QQ[x_1, x_2, x_3]
      p0=x_1*x_2, p1=x_1^2*x_2-x_1*x_3+x_3^3, p2=x_2^2*x_3+x_3, p3=-x_1*x_2;
      L={p0, p1, p2, p3}
      gmodsHeuristic(L, support(L))
  SeeAlso
///

doc ///
  Key
    (lazardProjection, List, RingElement)
    lazardProjection
  Headline
    Lazard projection with respect to a variable.
  Usage
    lazardProjection(L, v)
  Inputs
    L:List
      of polynomials all in the same ring.
    v:RingElement
      a variable in the ring.
  Outputs
    :List
      of projected polynomials not involving v.
  Description
    Text
      Lazard projection is an operation that takes a variable v and a set L of polynomials in $n$ variables, and returns a set of polynomials 
      in the remaining $n-1$ variables, representing the significant points of the polynomials.
      This is used in the projection phase of Cylindrical Algebraic Decomposition, and consists of the leading and trailing coefficients of the given 
      polynomials w.r.t v, the discriminants of the polynomials w.r.t v and the resultants between each pair of polynomials 
      w.r.t v. For openCAD, the trailing coefficients are not needed.
    Example
      R=QQ[x_1, x_2, x_3]
      p0=x_1*x_2, p1=x_1^2*x_2-x_1*x_3+x_3^3, p2=x_2^2*x_3+x_3;
      L={p0, p1, p2}
      L2 = lazardProjection(L, x_1)
  SeeAlso
    leadCoefficient
    factorsInList
  Subnodes
    factorsInList
///

doc ///
  Key
    (projectionPhase, List)
    projectionPhase
  Headline
    Full Lazard projection of list of polynomials.
  Usage
    projectionPhase(L)
  Inputs
    L:List
      of polynomials in a ring.
  Outputs
    S:List
      of lists of projection polynomials in increasing numbers of variables (starting with univariate polynomials and ending in the original list L).
    ordering:List
      of variables used in projections. The projection set of polynomials of in k variables will contain the first k variables of this list.
  Description
    Text
      The projection phase of the CAD is calculated. Given a list L of polynomials in $n$ variables (level $n$), the Lazard projection is applied recursively
      until one variable remains. At each step, the list of projection polynomials and the projected variable are stored, resulting in a final list of projection 
      polynomials from level 1 to level $n$, and the list of variables, ordered so that the first $k$ variables of the list are the variables of the polynomials
      at level $k$.
    Example
      R=QQ[x_1, x_2, x_3]
      p0=x_1*x_2, p1=x_1^2*x_2-x_1*x_3+x_3^3, p2=x_2^2*x_3+x_3;
      L={p0, p1, p2}
      projectionPhase(L)
  SeeAlso
    gmodsHeuristic
    lazardProjection
  Subnodes
    gmodsHeuristic
    lazardProjection
///

doc ///
  Key
    (samplePoints, List)
    samplePoints
  Headline
    List of sample points representing open cells.
  Usage
    samplePoints(L)
  Inputs
    L:List
      nonempty, of polynomials in one variable.
  Outputs
    SP:List
      of points in QQ.
  Description
    Text
      Sample points are the representative points in each cell of the CAD. Such points are computed in the lifting phase, by isolating real 
      roots of the univariate polynomials obtained by substituting in sample points from lower levels.
      
      This method relies on the interval bisection method from @TO "RealRoots::realRootIsolation"@ in the @TO "RealRoots::RealRoots"@ package, which isolates the real roots within a specific half-open interval.
      If two intervals touch on a root, the interval bisection is run again with more precision until no intervals touch on a root.
      Once this is completed, it takes the midpoint of each interval as a sample point for each open region, along with points higher and lower than the largest and smallest,
      representing the first and last open cells.
    Example
      R=QQ[x]
      p0=x^2-1, p1=x^3-1;
      L1={p0, p1}
      samplePoints(L1)

      p2=5*x^3+1, p3=x^2-1, p4=1/2*x^5+3*x-1;
      L2={p2, p3, p4}
      samplePoints(L2)
  SeeAlso
  Subnodes
///

doc ///
  Key
    (liftingPoint, List, List, MutableHashTable)
    liftingPoint
  Headline
    OpenCAD above the point given.
  Usage
    liftingPoint(S, ordering, alpha)
  Inputs
    S:List
      of lists of RingElements, representing the projection polynomials of each level.
    ordering:List
      the variable ordering followed in the projection.     
    alpha:MutableHashTable
      the point described using a hash table where the keys are RingElements (variables) and the values are sample points.
  Outputs
    LP:MutableHashTable
      describing an OpenCAD.
  Description
    Text
      Given the projection phase of a CAD (S), liftingPoint creates an Open Cylindrical Algebraic Decomposition, which breaks the space into cells where 
      the signs of the polynomials in each element of S are constant.
    Example
      R=QQ[x_1, x_2, x_3]
      p0=x_1*x_2, p1=x_1^2*x_2-x_1*x_3+x_3^3, p2=x_2^2*x_3+x_3;
      L={p0, p1, p2}
      alpha = new MutableHashTable
      alpha#(x_2) = -2, alpha#(x_3) = -3/32;
      (S, ordering) =  projectionPhase(L)
      LP = liftingPoint(S, ordering, alpha)
      hashify LP
  SeeAlso
    evaluatePolynomials
    samplePoints
  Subnodes
    evaluatePolynomials
    samplePoints
///

doc ///
  Key
    (openCAD, List)
    openCAD
  Headline
    Open CAD of listed polynomials.
  Usage
    openCAD(L)
  Inputs
    L:List
      of polynomials all in the same ring.
  Outputs
    C:MutableHashTable
      describing an open CAD of the given list of polynomials.
  Description
    Text
      An open CAD is a mathematical object that decomposes the space into cells in which the given polynomials are sign invariant.
    Example
      R=QQ[x_1, x_2, x_3]
      p0=x_1*x_2, p1=x_1^2*x_2-x_1*x_3+x_3^3, p2=x_2^2*x_3+x_3;
      L={p0, p1, p2}
      openCAD(L)
      hashify openCAD(L)
      
      R=QQ[x_1, x_2]
      p0=x_1-x_2, p1=x_1^3+x_2^2;
      L={p0, p1}
      openCAD(L)
      hashify openCAD(L)
  SeeAlso
    projectionPhase
    liftingPoint
  Subnodes
    projectionPhase
    liftingPoint
///

doc ///
  Key
    (positivePoint, List, MutableHashTable)
    positivePoint
  Headline
    Checks if there is a point above the cell where all polynomials are positive.
  Usage
    positivePoint(L, cell)
  Inputs
    L:List
      a list of polynomials.
    cell:MutableHashTable
      the cell of the CAD.
  Outputs
    PP:MutableHashTable
      describing a point in the cell (evaluations of all variables) where all polynomials in L are strictly positive (if one exists).
  Description
    Text
      Given the a list of polynomials and a cell of a CAD, this method checks if a point exists where all polynomials are strictly positive, or returns null otherwise.
    Example
      R=QQ[x]
      p0=x^2-1, p1=x;
      L={p0, p1}
      C=openCAD(L);
      PP=positivePoint(L, C);
      hashify(PP)
  SeeAlso
  Subnodes
///

doc ///
  Key
    (findPositiveSolution, List)
    findPositiveSolution
  Headline
    Checks if there is a point where all given polynomials are positive.
  Usage
    findPositiveSolution(L)
  Inputs
    L:List
      a list of polynomials.
  Outputs
    :Boolean
      saying whether the CAD of L of has a point where all of the polynomials in the list are strictly positive.
    :HashTable
      describing a point in the cell (evaluations of all variables) where all polynomials in L are strictly positive (if one exists).
    :List
      describing the polynomial(s) evaluated at this point (if the point exists).
  Description
    Text
      Given a list of polynomials L, this checks if the CAD of L contains a point where each of the polynomials in L are strictly positive.
    Example
      R=QQ[x]
      p0=x^2-1, p1=x;
      L={p0, p1}
      FS=findPositiveSolution(L)
  SeeAlso
    openCAD
    positivePoint
  Subnodes
    openCAD
    positivePoint
///

doc ///
  Key
    hashify
    (hashify, HashTable)
    (hashify, BasicList)
    (hashify, Thing)
  Headline
    Recursively turns MutableHashTables into equivalent HashTables.
  Usage
    hashify(MHT)
  Inputs
    M:HashTable
      A (mutable) hash table.
    M:BasicList
      A (mutable) list.
    M:Thing
      Any other object that isn't one of the ones listed above.
  Outputs
    H:Thing
      wherein any mutable hash tables are replaced with equivalent hash tables.
  Description
    Text
      This method takes a MutableHashTable, HashTable, List or MutableList and turns any MutableHashTables within into HashTables, leaving everything else the same.
    Example
      R=QQ[x_1, x_2];
      M = new MutableHashTable from {-1_QQ=>new MutableHashTable from {-5/2=>new MutableHashTable from {"point"=>new MutableHashTable from {x_1=>-1_QQ, x_2=>-5/2}}}};
      hashify M
  SeeAlso
    evaluatePolynomials
    liftingPoint
    openCAD
    positivePoint
    findPositiveSolution
///

TEST /// -* factorsInList test *-
-- Test 0
  R=QQ[x_1, x_2, x_3]
  p0=x_1*x_2, p1=x_1^2*x_2-x_1*x_3+x_3^3, p2=x_2^2*x_3+x_3;
  L={p0, p1, p2}
  F = factorsInList(L)
  answer = {x_2, x_1, x_1^2*x_2+x_3^3-x_1*x_3, x_3, x_2^2+1}
  assert(sort F === sort answer)
///

TEST /// -* evaluatePolynomials test *-
-- Test 1
  R=QQ[x_1, x_2, x_3]
  p=x_1^2*x_2-x_1*x_3+x_3^3
  alpha = new MutableHashTable;
  alpha#(x_1) = 1, alpha#(x_2) = 3;
  E = evaluatePolynomials(p, alpha)
  assert(E == 3-x_3+x_3^3)
///

TEST /// -* evaluatePolynomials test (List)*-
-- Test 2
  R=QQ[x_1, x_2, x_3]
  p0=x_1*x_2, p1=x_1^2*x_2-x_1*x_3+x_3^3, p2=x_2^2*x_3+x_3;
  L={p0, p1, p2}
  alpha = new MutableHashTable
  alpha#(x_1) = 1, alpha#(x_2) = 3;
  E = evaluatePolynomials(L, alpha)
  assert(E == {3, 3-x_3+x_3^3, 9*x_3+x_3})
///

TEST /// -* leadCoefficient test *-
-- Test 3
  R=QQ[x_1, x_2, x_3]
  p=x_1^2*x_2-x_1*x_3+x_3^3
  L = leadCoefficient(p, x_1)
  assert(leadCoefficient(p, x_1) == x_2)
///

TEST /// -* gmodsHeuristic test *-
-- Test 4
  R=QQ[x_1, x_2, x_3]
  p0=x_1*x_2, p1=x_1^2*x_2-x_1*x_3+x_3^3, p2=x_2^2*x_3+x_3, p3=-x_1*x_2;
  L={p0, p1, p2, p3}  
  assert(gmodsHeuristic(L, support(L)) == x_1)
///

TEST /// -* lazardProjection test *-
-- Test 5
  R=QQ[x_1, x_2, x_3]
  p0=x_1*x_2, p1=x_1^2*x_2-x_1*x_3+x_3^3, p2=x_2^2*x_3+x_3;
  L={p0, p1, p2}
  LP = lazardProjection(L, x_1)
  assert(LP === {x_2, x_3, x_2^2+1, 4*x_2*x_3-1})
///

TEST /// -* projectionPhase test *-
-- Test 6
  R=QQ[x_1, x_2, x_3]
  p0=x_1*x_2, p1=x_1^2*x_2-x_1*x_3+x_3^3, p2=x_2^2*x_3+x_3;
  L={p0, p1, p2}
  PP = projectionPhase(L)
  answerS = {{x_2, x_2^2+1}, {x_2, x_3, x_2^2+1, 4*x_2*x_3-1}, {x_2, x_1, x_1^2*x_2+x_3^3-x_1*x_3, x_3, x_2^2+1}}
  answerordering = {x_2, x_3, x_1}
  assert(PP == (answerS, answerordering))
///

TEST /// -* samplePoints test *-
-- Test 7
  R=QQ[x]
  p0=x^2-1, p1=x^3-1
  L={p0, p1}
  SP = samplePoints(L)
  assert(SP == {-3, -1/2, 2})
///

TEST /// -* liftingPoint test *-
-- Test 8
  R=QQ[x_1, x_2, x_3]
  p0=x_1*x_2, p1=x_1*x_2+x_3^2;
  L={p0, p1}
  (S, ordering) = projectionPhase(L)
  alpha = new MutableHashTable
  alpha#(x_3) = -1_QQ, alpha#(x_1) = 1_QQ;
  LP = liftingPoint(S, ordering, alpha)

  cellLevelThreeA = new MutableHashTable from {"point"=>new MutableHashTable from {x_3=>-1_QQ, x_1=>1_QQ, x_2=>-3/4}}
  cellLevelThreeB = new MutableHashTable from {"point"=>new MutableHashTable from {x_3=>-1_QQ, x_1=>1_QQ, x_2=>-5/2}  }
  cellLevelThreeC = new MutableHashTable from {"point"=>new MutableHashTable from {x_3=>-1_QQ, x_1=>1_QQ, x_2=>1_QQ}}  

  cellLevelTwo = new MutableHashTable from {-3/4_QQ=>cellLevelThreeA, -5/2_QQ=>cellLevelThreeB, 1_QQ=>cellLevelThreeC, "point"=>new MutableHashTable from {x_3=>-1_QQ, x_1=>1_QQ}, "polynomials"=>{x_2, 1_QQ, x_2+1}}

  assert(hashify(LP) === hashify(cellLevelTwo))
///

TEST /// -* openCAD test *-
-- Test 9
  R=QQ[x_1, x_2]
  p0=x_1^2+x_2, p1=x_1^3*x_2^2;
  L={p0, p1}
  C=openCAD(L)

  cellLevelThreeA = new MutableHashTable from {"point"=>new MutableHashTable from {x_1=>-1_QQ, x_2=>-5/2}}
  cellLevelThreeB = new MutableHashTable from {"point"=>new MutableHashTable from {x_1=>-1_QQ, x_2=>-3/4} }
  cellLevelThreeC = new MutableHashTable from {"point"=>new MutableHashTable from {x_1=>-1_QQ, x_2=>1_QQ}}
  cellLevelThreeD = new MutableHashTable from {"point"=>new MutableHashTable from {x_1=>1_QQ, x_2=>-5/2}}
  cellLevelThreeE = new MutableHashTable from {"point"=>new MutableHashTable from {x_1=>1_QQ, x_2=>-3/4}}
  cellLevelThreeF = new MutableHashTable from {"point"=>new MutableHashTable from {x_1=>1_QQ, x_2=>1_QQ}}
  
  ptLevelTwoA = new MutableHashTable from {-5/2=>cellLevelThreeA, -3/4=>cellLevelThreeB, 1_QQ=>cellLevelThreeC, "point"=>new MutableHashTable from {x_1=>-1_QQ}, "polynomials"=>{x_2+1, x_2, -1_QQ}}
  ptLevelTwoB = new MutableHashTable from {-5/2=>cellLevelThreeD, -3/4=>cellLevelThreeE, 1_QQ=>cellLevelThreeF, "point"=>new MutableHashTable from {x_1=>1_QQ}, "polynomials"=>{x_2+1, x_2, 1_QQ}}  
  ptLevelTwoC = new MutableHashTable
  
  cellLevelOne = new MutableHashTable from {-1_QQ=>ptLevelTwoA, 1_QQ=>ptLevelTwoB, "point"=>ptLevelTwoC, "polynomials"=>{x_1}}
  
  assert(hashify cellLevelOne === hashify C)
  
///

TEST /// -* positivePoint test 1*-
-- Test 10
  R=QQ[x_1, x_2, x_3]
  p0=x_1*x_2, p1=x_1^2*x_2-x_1*x_3+x_3^3, p2=x_2^2*x_3+x_3, p3=-x_1*x_2;
  L={p0, p1, p2, p3};
  C=openCAD(L)
  PP=positivePoint(L, C)
  assert(PP == null)
/// 
  
TEST /// -* positivePoint test 2*-
-- Test 11
  R=QQ[x]
  p0=x^2-1, p1=x;
  L={p0, p1};
  C=openCAD(L)
  PP=positivePoint(L, C)
  answer = new MutableHashTable from {x => 2_QQ}
  assert(hashify PP === hashify answer)
///

TEST /// -* findPositiveSolution test 1*-
-- Test 12
  R=QQ[x_1, x_2, x_3]
  p0=x_1*x_2, p1=x_1^2*x_2-x_1*x_3+x_3^3, p2=x_2^2*x_3+x_3;
  L={p0, p1, p2}
  PP = new HashTable from {x_2=>1_QQ, x_3=>5/4, x_1=>1_QQ};
  assert(findPositiveSolution L === (true, PP))
///

TEST /// -* findPositiveSolution test 2*-
-- Test 13
  R=QQ[x_1, x_2, x_3]
  p0=x_1*x_2, p1=x_1^2*x_2-x_1*x_3+x_3^3, p2=x_2^2*x_3+x_3, p3=-x_1*x_2;
  L={p0, p1, p2, p3}
  assert(findPositiveSolution L === (false, null))  
///

TEST /// -* findPositiveSolution test 3*-
-- Test 14
  R=QQ[x_1, x_2, x_3]
  p0=x_1*x_2, p1=x_1^2*x_2-x_1*x_3+x_3^3, p2=x_2^2*x_3+x_3, p3=-x_1*x_2;
  L={p0, p1, p2, p3}
  assert(findPositiveSolution L === (false, null))
/// 
  
TEST /// -* findPositiveSolution test 4*-
-- Test 15
  R=QQ[x]
  p0=x^2-1, p1=x;
  L={p0, p1}
  PP = new HashTable from {x => 2_QQ};
  assert(findPositiveSolution L === (true, PP))
///

TEST /// -* hashify test*-
-- Test 16
  R=QQ[x_1, x_2]
  MCell = new MutableHashTable from {-1_QQ=>new MutableHashTable from {-5/2=>new MutableHashTable from {"point"=>new MutableHashTable from {x_1=>-1_QQ, x_2=>-5/2}}}}
  HCell = new HashTable from {-1_QQ=>new HashTable from {-5/2=>new HashTable from {"point"=>new HashTable from {x_1=>-1_QQ, x_2=>-5/2}}}}
  assert(hashify MCell === HCell)
///

end--
