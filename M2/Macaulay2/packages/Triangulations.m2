newPackage(
        "Triangulations",
        Version => "0.2", 
        Date => "25 Oct 2024",
        Authors => {{
                Name => "Mike Stillman", 
                Email => "mike@math.cornell.edu", 
                HomePage=>"http://www.math.cornell.edu/~mike"
                }},
        Headline => "triangulations of polyhedra and point sets",
        Keywords => {"Combinatorics"},
        PackageImports => {"FourierMotzkin"},
        PackageExports => {
            "Topcom", 
            "Polyhedra" -- really only needed for `regularSubdivision`?
            },
        DebuggingMode => false
        )

export {
    -- types defined here
    "Triangulation",
    "Chirotope",

    "triangulation",
    "vectors",
    "regularFineTriangulation",
    "chirotope",
    "naiveChirotope",
    "flips",
    "bistellarFlip",
    "neighbors",
    "generateTriangulations",
    "allTriangulations",
    "isStar",
    "isFine",
    "isRegularTriangulation", -- note the non-use of isRegular.  Is this ok?
    "regularTriangulationWeights",
    
    "affineCircuits",
    "volumeVector",
    "gkzVector",
    
    "delaunayWeights",
    "delaunaySubdivision",

    "fineStarTriangulation",
    "regularFineStarTriangulation",
    "naiveIsTriangulation",
    
    "ConeIndex"
    }

augment = method()
augment Matrix := (A) -> (
    -- A is a matrix over ZZ
    -- add in a last row of 1's.
    n := numColumns A;
    ones := matrix {{n : 1}};
    A || ones
    )

-- also defined:
-- isRegularTriangulation T
-- regularTriangulationWeights T
-- naiveIsTriangulation T

-- TODO: design decision:
--   The "rays" or "vertices": stored as matrix or list?
--   Store list as vector configuration (i.e. homogenized?)
--   Accessors: max, rays?
Triangulation = new Type of HashTable
Chirotope = new Type of HashTable

net Triangulation := T -> "triangulation " | net max T
toString Triangulation := T -> toString max T
toExternalString Triangulation := T -> "triangulation(transpose matrix" | toExternalString vectors T | "," | toExternalString max T | ", Homogenize => false)"
--expression Triangulation := X -> (describe Triangulation)#0
--describe Triangulation := X -> describe max X


-- XXX working on this 15 June 2022.
-- Change all uses of fineStarTriangulation to return 
-- Assumptions: A is a dxn matrix over the integers, or rationals.
--              convexHull A 
-- all columns of A should be boundary points (with the possible exception of the last column).
-- A can either contain the interior point to cone over, or not.
-- If it does contain it, I believe that no element of tri should use that index.

-- Returns: a new triangulation of the point set A | 0.
--  NOTE: not yet!  it currently returns a list of subsets, such that if one appends to each subset 
--    the number "numcols A", this will be a star triangulation.
--  NOTE: I'm not sure why this would need to be regular either...

fineStarTriangulation = method(Options => {ConeIndex => null})
fineStarTriangulation(Matrix, List) := List => opts -> (A, tri) -> (
    coneindex := if opts.ConeIndex === null then 
                     numcols A -- in this case, assume that the cone point is a new index, which would be 'numcols A'
                 else if instance(opts.ConeIndex, ZZ) and opts.ConeIndex >= 0 and opts.ConeIndex <= numcols A + 1 then
                     opts.ConeIndex
                 else
                     error "ConeIndex must be an index from 0 .. number of columns + 1...";
    aA := augment A;
    -- H := first halfspaces convexHull aA;
    H := transpose(-(first fourierMotzkin aA));
    myfacets := for e in entries H list (
        positions(flatten entries(matrix {e} * aA), x -> x == 0)
        );
    sort unique flatten for f in tri list for g in myfacets list (
        a := toList(set g * set f); 
        if #a < numRows A then 
            continue 
        else 
            sort append(a, coneindex)
        )
    -- newtri = for f in newtri list append(f, numColumns A)
    )

-- TODO: is this really a regular triangulation?
-- I think it might be, as long as the cone index is not used in regularFineTriangulation.
regularFineStarTriangulation = method(Options => options fineStarTriangulation)
regularFineStarTriangulation Matrix := List => opts -> (A) -> fineStarTriangulation(A, topcomRegularFineTriangulation A, opts)

-- TODO: I am not sure that this is correct.
naiveIsTriangulation = method()
naiveIsTriangulation(Matrix, List, List) := (A, circuits, tri) -> (
    aA := augment A;
    -- H := first halfspaces convexHull aA;
    H := transpose(-(first fourierMotzkin aA));
    myfacets := for e in entries H list (
        positions(flatten entries(matrix {e} * aA), x -> x == 0)
        );
    -- test 1: each wall should be in a facet of the convex hull, or occur exactly twice.
    -- This is NOT correct?!
    walls := tally flatten for t in tri list subsets(t,#t-1);
    test1 := for k in keys walls list (
        if any(myfacets, f -> isSubset(k,f)) then 
          walls#k == 1
        else
          walls#k == 2
        );
    if any(test1, x -> not x) then return false;
    -- test 2: for each oriented circuit Z = (Z+, Z-)
    test2 := for z in circuits list (
      # select(tri, t -> isSubset(z_0, t)),
      # select(tri, t -> isSubset(z_1, t))
      );
    all(test2, x -> x#0 == 0 or x#1 == 0)
    )
naiveIsTriangulation(Matrix, List) := (A, tri) -> naiveIsTriangulation(A, orientedCircuits A, tri)



-- Allow both rays and points, i.e. homogenized A or not
triangulation = method(Options => {Homogenize => true}) -- true means this is a point set.
triangulation(Matrix, List) := Triangulation => opts -> (A, tri) -> (
    -- should we check basic things?  e.g. tri is a list of
    -- lists of integers in the range 0..numcols A - 1.
    -- and that A is a matrix over ZZ or QQ?
    if ring A =!= ZZ and ring A =!= QQ then
        error "expected matrix over ZZ or QQ";
    n := numcols A;
    if not all(tri, f -> all(f, p -> instance(p, ZZ))) then 
        error "expected a list of list of integers";
    -- should we sort the sets?  Probably...
    A1 := if opts.Homogenize then augment A else A;
    vecs := transpose entries A1;
    sorted := tri//sort/sort; -- this sorts the triangulation
    T := new Triangulation from {
        cache => new CacheTable,
        symbol vectors => vecs,
        symbol max => sorted
        };
    T.cache#"point set" = opts.Homogenize;
    T.cache.matrix = A1;
    T
   )

vectors = method()
vectors Triangulation := List => T -> T.vectors
max Triangulation := List => T -> T.max
matrix Triangulation := opts -> T -> T.cache.matrix

isWellDefined Triangulation := Boolean => T -> (
    topcomIsTriangulation(matrix T, max T, Homogenize => false)
    )

Triangulation == Triangulation := Boolean => (S, T) -> S === T

naiveIsTriangulation Triangulation := Boolean => T -> (
    naiveIsTriangulation(matrix T, max T) -- BUG: needs to take Homogenize?
    )

-- The following is currently only for point sets
-- and is probably slow for larger triangulations too.
isTriangulation = method()
isTriangulation(Matrix, List) := (M, tri) -> (
    -- for the moment, we assume that M is a point set.
    d := #(tri#0) - 1;
    P := convexHull M;
    M' := M || matrix{{numcols M: 1}};
    volP := d! * volume P;
    volP2 := sum for t in tri list abs det M'_t;
    if volP != volP2 then (
        << "volume is not correct: " << volP << " != " << volP2 << endl;
        return false;
        );
    simplices := hashTable for t in tri list t => convexHull M_t;
    for x in subsets(keys simplices, 2) do 
        if dim(intersection(simplices#(x#0), simplices#(x#1))) == d then (
            << "simplices " << x << " intersect in full dimensional region" << endl;
            return false;
            );
    true
    )

isRegularTriangulation = method(Options => {Homogenize => true})
isRegularTriangulation Triangulation := Boolean => opts -> T -> (
    topcomIsRegularTriangulation(T.cache.matrix, max T, Homogenize => false)
    )
isRegularTriangulation(Matrix, List) := Boolean => opts -> (A, tri) -> (
    topcomIsRegularTriangulation(A, tri, opts)
    )

regularTriangulationWeights = method(Options => options isRegularTriangulation)
regularTriangulationWeights Triangulation := List => opts -> T -> (
    topcomRegularTriangulationWeights(matrix T, max T, Homogenize => false)
    )
regularTriangulationWeights(Matrix, List) := List => opts -> (A, tri) -> (
    topcomRegularTriangulationWeights(A, tri, opts)
    )

regularFineTriangulation = method(Options => options isRegularTriangulation)
regularFineTriangulation Matrix := Triangulation => opts -> (A) -> (
    tri := topcomRegularFineTriangulation(A, opts);
    if tri === null then null else 
        triangulation(A, tri, opts)
    )

-- TODO/BUG: this ASSUMES (A, tri) is a triangulation.
-- TODO: add in Homogenize as an option?
isFine = method()
isFine(Matrix, List) := Boolean => (A, tri) -> (
    numcols A == tri//flatten//unique//length
    )

-- TODO/BUG: this ASSUMES (A, tri) is a triangulation.
isStar = method()
isStar(Matrix, List) := Boolean => (A, tri) -> (
    -- assumption?  last column of A is the zero element?  (or the one the star is taken with respect to).
    origin := numcols A - 1;
    all(tri, t -> member(origin, t))
    )

isStar Triangulation := Boolean => T -> isStar(matrix T, max T)
isFine Triangulation := Boolean => T -> isFine(matrix T, max T)

allTriangulations = method(Options => options topcomAllTriangulations)
allTriangulations Matrix := List => opts -> A -> (
    tris := topcomAllTriangulations(A, opts);
    for t in tris list triangulation(A, t, Homogenize => opts.Homogenize)
    )

delaunayWeights = method()
delaunayWeights Matrix := (A) -> (
    matrix{for i from 0 to numcols A - 1 list ((transpose A_{i}) * A_{i})_(0,0)}
    )

delaunaySubdivision = method()
delaunaySubdivision Matrix := A -> elapsedTime regularSubdivision(A, elapsedTime delaunayWeights A)

-----------------------------------------------------------
-- Chirotope code.  This could potentially go elsewhere? --
-----------------------------------------------------------

chirotope = method(Options => true)

chirotope String := {} >> opts -> s -> (
    new Chirotope from {
        cache => new CacheTable,
        symbol String => s
        }
    )

toString Chirotope := String => OM -> OM.String

chirotope Matrix := Chirotope => {Homogenize => true} >> opts -> A -> (
    chirotope chirotopeString(A, opts)
    )

naiveChirotope = method(Options => true)
naiveChirotope Matrix := Chirotope => {Homogenize => true} >> opts -> A -> (
    chirotope naiveChirotopeString(A, opts)
    )

Chirotope == Chirotope := Boolean => (C, D) -> toString C === toString D

--------------------------------------
-- Flips and triangulations ----------
--------------------------------------
-- This code is not an interface to topcom.
--   This is my own code, which used to be 
--   part of `StringTorics`, but seems more 
--   appropriate in a `Triangulations` package.
-- It is much slower than topcom, but allows to collect a certain number of triangulations.
-- TODO: make an interface that yields the next triangulation when called? (matching python usage).
--------------------------------------
sortTriangulation = method()
sortTriangulation List := (T) -> sort for t in T list sort t

-- TODO: why codim2 and codim2s?
codim2 = (tri) -> (
    -- find all pairs whose intersection is all but one of the vertices.
    n := #tri_0;
    select(subsets(tri, 2), v -> length toList ((set (v#0)) * (set (v#1))) == n-1)
    )

codim2s = (tri) -> (
    n := #tri_0;
    C2 := unique apply(subsets(tri, 2), v -> sort toList (set v#0 + set v#1));
    select(C2, x -> #x == n+1)
    )

affineCircuits = method()

-- TODO: RENAME to flips, or possibleFlips
-- previously in triangulations-code.m2, named affineCircuits
affineCircuits(Matrix, List) := (Amat, tri) -> (
    -- tri: a set of subsets of size d from 0..#columns(Amat)-1, that form
    --  a triangulation of conv(columns of Amat).
    -- Amat: d-1 x n matrix over ZZ or QQ of the points, with the d-1 rows independent.
    --   OR of size d x n, with the last row all 1's. (or the rows are rank (d-1)).
    d := #(tri#0);
    assert all(tri, t -> #t == d);
    if numrows Amat == d-1 then
      Amat = Amat || matrix{ toList(numcols Amat : 1) };
    c2 := codim2s tri;
    unique for c in c2 list (
        z := flatten entries syz Amat_c;
        rsort {c_(positions(z, zi -> zi > 0)), c_(positions(z, zi -> zi < 0))}
        )
    )

affineCircuits Triangulation := T -> affineCircuits(matrix T, max T)

-- previously in triangulations-code.m2,
link = method()
link(List, List) := (tau, triangulation) -> (
    S := select(triangulation, t -> isSubset(tau, t));
    sort for s in S list sort toList (set s - set tau)
    )

flips = method(Options => options topcomFlips)
flips Triangulation := List => opts -> T -> (
    topcomFlips(matrix T, max T, Homogenize => false, RegularOnly => opts.RegularOnly)
    )

bistellarFlip = method()

-- previously in triangulations-code.m2, named flip.
bistellarFlip(List,List) := (tri, affineCircuit) -> (
    -- returns a new triangulation
    -- input: tri: a list of lists of integers (a triangulation).
    --        affineCircuit: a pair of lists of integers.  Each is disjoint, the negative/positive part of the affine circuit.
    -- a. start with an affine circuit
    -- 1. compute 2 choices of triangulation for the circuit
    whole := sort unique flatten affineCircuit;
    T1 := for i in affineCircuit#0 list sort toList (set whole - set {i});
    T2 := for i in affineCircuit#1 list sort toList (set whole - set {i});
    -- 2. which is contained in tri?
    -- XXXXXXXXXXXX
    S1 := unique for tau in T1 list link(tau, tri);
    S2 := unique for tau in T2 list link(tau, tri);
    if #S1 > 1 or #S2 > 1 then (
        --<< "link is not unique, here they are: S1: " << S1 << " S2: " << S2 << endl;
        return null;
        );
    S1 = first S1;
    S2 = first S2;
    if #S1 > 0 and #S2 > 0 then (
        error "my logic is wrong: somehow both links exist...";
        return null;
        );
    if #S1 == 0 then (
        S1 = S2;
        (T1,T2) = (T2,T1);
        );
    -- Now we can change the triangulation:
    T := set tri;
    outgoing := set flatten for s in S1 list for tau in T1 list sort join(s,tau);
    incoming := set flatten for s in S1 list for tau in T2 list sort join(s,tau);
    --<< "outgoing: " << sort toList outgoing << endl;
    --<< "incoming: " << sort toList incoming << endl;    
    sortTriangulation toList(T - outgoing + incoming)
    )

bistellarFlip(Triangulation, List) := (T, affineCircuit) -> (
    tri := bistellarFlip(max T, affineCircuit);
    if tri === null then null else
        triangulation(matrix T, tri, Homogenize => false)
    )

neighbors = method()
neighbors Triangulation := List => T -> (
    -- each element of the result is of the form:
    -- {circuit, triangulation}
    -- only circuits that do not change the support of the triangulation are included.
    circuits0 := affineCircuits T;
    circuits := select(circuits0, z -> #z#0 > 1 and #z#1 > 1);
    for c in circuits list (
        T1 := bistellarFlip(T, c);
        if T1 === null then continue else {c, T1}
        )
    )

generateTriangulations = method(Options => {Limit=>infinity, RegularOnly=>false, Homogenize => true})
generateTriangulations Triangulation := opts -> T -> (
    Amat := matrix T;
    allT := new MutableHashTable;
    allT#T = true;
    TODO := {T};
    while #TODO > 0 and #(keys allT) < opts.Limit do (
        nextTRI := TODO#0;
        TODO = drop(TODO,1);
        flips := select(affineCircuits nextTRI, z -> #z#0 > 1 and #z#1 > 1);
        fliptris := for f in flips list bistellarFlip(nextTRI, f);
        newT := select(fliptris, x -> x =!= null);
        for T in newT do (
            if not allT#?T then (
                --<< "new triangulation: " << T << endl;
                if not opts.RegularOnly or isRegularTriangulation T then (
                    allT#T = true;
                    TODO = append(TODO, T);
                    );
                ));
        if debugLevel > 0 then 
            << "todo = " << #TODO << " and #triang = " << #(keys allT) << endl;
        );
    keys allT
    )

-- TODO? need Homogenize?
generateTriangulations Matrix := opts -> Amat -> (
    generateTriangulations(regularFineTriangulation Amat, opts)
    )
generateTriangulations(Matrix, List) := opts -> (Amat, triang) -> (
    tris := generateTriangulations(triangulation(Amat, triang, Homogenize => opts.Homogenize), 
        Limit => opts.Limit, RegularOnly => opts.RegularOnly);
    tris/max -- strip the triangulation class from these.  This matches (up to permutation) output of allTriangulations
    )

volumeVector = method()
volumeVector (Matrix, List) := (Amat, tri) -> (
    if #tri == 0 then error "expected at least one simplex";
    nelems := #tri#0;
    d := nelems-1;
    if not all(tri, f -> #f == nelems)
    then error "expected a triangulation";
    if numrows Amat =!= nelems then Amat = Amat || matrix{{(numcols Amat):1}};
    if numrows Amat =!= nelems then error "triangulation not compatible with matrix";
    H := hashTable for t in tri list t => (abs det Amat_t)/d!;
    if debugLevel > 0 then
        << "Volume = " << (sum values H)/d! << endl;
    for i from 0 to numColumns Amat - 1 list (
        T := select(tri, t -> member(i,t));
        sum for t in T list H#t
        )
    )
volumeVector Triangulation := T -> volumeVector(matrix T, max T)

gkzVector = volumeVector

beginDocumentation()

-*
      needsPackage "StringTorics"
      topes = kreuzerSkarke(5, Limit => 10)
      Q = cyPolytopeData topes_7
      isFavorable Q
      rays Q
      A = matrix topes_7
      P2 = polar convexHull A
      Amat = latticePointList P2

      -- one from h11=7
      --LP = {{-1, -1, -1, 1}, {-1, -1, -1, 2}, {-1, -1, 0, 1}, {-1, 0, -1, 1}, {-1, 0, 1, 0}, {-1, 0, 2, 0}, {-1, 1, 0, 0}, {-1, 2, 0, -1}, {0, -1, -1, 1}, {0, 1, 1, -1}, {2, 0, 0, -1}, {0,0,0,0}}      
*-

doc ///
  Key
    Triangulations
  Headline
    generating and manipulating triangulations of point or vector configurations
  Description
    Text
      {\bf Warning!} This package is experimental, documentation is missing,
      and the interface will be cleaned up and changed.  Use only if these issues
      don't bother you!
    Text
      @SUBSECTION "Data of a triangulation"@
    Text
      @UL {
          TO (max, Triangulation),
          TO (vectors, Triangulation)
          }@
    Text
      @SUBSECTION "Creating triangulations"@
    Text
      @UL {
          TO (triangulation, Matrix, List),
          TO (regularFineTriangulation, Matrix),
          TO (generateTriangulations, Triangulation),
          TO (allTriangulations, Matrix)
          }@
    Text
      @SUBSECTION "Properties of triangulations"@
    Text
      @UL {
          TO (isWellDefined, Triangulation),
          TO (isRegularTriangulation, Triangulation),
          TO (regularTriangulationWeights, Triangulation),
          TO (isStar, Triangulation),
          TO (isFine, Triangulation),
          TO (naiveIsTriangulation, Triangulation)
          }@
    Text
      @SUBSECTION "Exploring the set of triangulations"@
    Text
      @UL {
          TO (bistellarFlip, Triangulation, List),
          TO (flips, Triangulation),
          TO (neighbors, Triangulation),
          TO (affineCircuits, Triangulation),
          TO (volumeVector, Triangulation),
          TO (gkzVector, Triangulation),
          TO (delaunayWeights, Matrix),
          TO (delaunaySubdivision, Matrix)
          }@
    Text
      This package is designed to help compute and explore the set of all (or many) triangulations
      of a point set or polytope.

      We give a sample use of this package.
    Example
      LP = {{-1, 0, -1, 1}, {-1, 0, 1, 0}, {-1, 0, 2, -1}, {-1, 1, -1, 0}, {1, 0, -1, 0}, {1, 0, 1, 0}, {2, -1, -1, 0}, {0, 0, 1, 0}, {1, 0, 0, 0}, {0,0,0,0}}      
      A = transpose matrix LP
      elapsedTime Ts = allTriangulations(A, Fine => true);
      select(Ts, T -> isStar T)
      #oo == 1
      #Ts == 51
      
      T = regularFineTriangulation A
      elapsedTime Ts2 = generateTriangulations T;
      #Ts2 == #Ts
  SeeAlso
    "Polyhedra::Polyhedra"
    "Topcom::Topcom"
    "ReflexivePolytopesDB::ReflexivePolytopesDB"
///

doc ///
  Key
    isRegularTriangulation
    (isRegularTriangulation, Triangulation)
  Headline
    determine if a given triangulation is a regular triangulation
  Usage
    isRegularTriangulation T
  Inputs
    T:Triangulation
      A triangulation of a point or vector configuration
    Homogenize => Boolean
      unused for this method
  Outputs
    :Boolean
      whether the given triangulation is regular
  Description
    Text
      A triangulation is called regular if it can be constructed in the following way: place the 
      point set in one higher dimension at various heights in the new variable.  Compute the
      convex hull.  Collect the list of facets with downward pointing normal (last coordinate of normal vector
      is negative).  If each of these is a simplex, then these form a triangulation of the
      original point set.  A triangulation which arises this way is called {\it regular}.  See 
      the book [deLoera et al] for more details and many beautiful properties of such triangulations.
    Text
      The following example is one of the simplest examples of a non-regular
      triangulation.  Notice that {\tt tri} is a triangulation of the 
      polytope which is the convex hull of the columns of $A$, which are 
      the only points allowed in the triangulation.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      tri = {{0,1,2}, {1,3,5}, {2,3,4}, {0,1,5}, 
          {0,2,4}, {3,4,5}, {1,2,3}}
      T = triangulation(A, tri)
    Text
      We check that {\tt T} is indeed a triangulation, and whether it is a regular triangulation.
    Example
      isWellDefined T
      isRegularTriangulation T
    Text
      Many of the functions in this package are wrappers for topcom functions.
      Setting the global variable {\tt debugLevel} to either 1,2, or 5 will give more detail about
      what files are written to Topcom, and what the executable is.
      Setting {\tt debugLevel} to 0 means that the function will run silently.
  Caveat
    Does topcom check that the triangulation is actually well defined?  I'm not sure...  This is why we call
    @TO (isWellDefined, Triangulation)@ first.
  SeeAlso
    (regularTriangulationWeights, Triangulation)
    regularFineTriangulation  
    (isWellDefined, Triangulation)
///

///
  Key
    neighbors
    (neighbors, Triangulation)
  Headline
    find the neighbors of a triangulation of a point or vector set
  Usage
    neighbors T
    XXX
  Inputs
    T:Triangulation
  Outputs
    :List
      of lists of size 2: the first element is a circuit, and the second is @ofClass Triangulation@
  Description
    Text
      The neighbors of a triangulation $T$ are those triangulations which are related to $T$ 
      by a bistellar flip.  Some flips can change the set of vertices (the support) of a triangulation.
      This function {\bf does not} consider these.  Thus, if the triangulation is fine, the neighbors 
      returned from this function are all fine triangulations too.
    Example
      TODOwriteMe
  Caveat
  SeeAlso
///

doc ///
  Key
    triangulation
  Headline
    make a Triangulation object
  Usage
    triangulation(A, T)
  Inputs
    A:Matrix
    T:List
      representing a triangulation of the columns of $A$ (each element in the list
      is a list of indices in the range $0, \ldots, n-1$, where $n$ is the number of
      columns of $A$)
    Homogenize => Boolean
      if true, add a row of ones to think of this as a vector configuration in one higher dimension.
  Outputs
    :Triangulation
      A @TO Triangulation@ object.  Very little computation is performed.  The matrix and list representing
      a triangulation is packaged into an object to make clear that it is a triangulation
  Description
    Text
    Example
      P = hypercube 3
      A = vertices P
      T = topcomRegularFineTriangulation A
      tri = triangulation(A, T)
      matrix tri
      vectors tri
      max tri
      isWellDefined tri
      netList affineCircuits tri
      isFine tri
      isStar tri
      isRegularTriangulation tri
  Caveat
  SeeAlso
    (max, Triangulation)
    (vectors, Triangulation)
    regularFineTriangulation
    (isWellDefined, Triangulation)
///

doc ///
  Key
    (allTriangulations, Matrix)
    allTriangulations
    [allTriangulations, ConnectedToRegular]
    [allTriangulations, Fine]
    [allTriangulations, Homogenize]
    [allTriangulations, RegularOnly]
  Headline
    use topcom to generate all triangulations of a point or vector configuration
  Usage
    allTriangulations A
    allTriangulations(A, Homogenize => true, Fine => true, RegularOnly => true)
  Inputs
    A:Matrix
    Homogenize => Boolean
    ConnectedToRegular => Boolean
    Fine => Boolean
    RegularOnly => Boolean    
  Outputs
    :List
  Description
    Text
      This function constructs all triangulations of the point set corresponding to $A$
      (or triangulation of the cone over $A$, if {\tt Homogenize => false} is given.
      With no optional arguments, the default is to construct all regular triangulations.
      
      This function is a wrapper for the topcom function @TO "Topcom::topcomAllTriangulations"@,
      and has the same optional arguments as that function.
      This function returns a list of @TO Triangulation@'s.
      
      A triangulation is a list of list of the indices of the maximal simplices in the triangulation.
      (the index of the point corresponding to the $i$-th column (starting at $0$) is simply $i$.
      
      For example, the following point set is the smallest which has a non-regular triangulation.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      Ts = allTriangulations A
      #Ts == 16
      netList Ts
      tri = Ts#0
      isWellDefined tri
      isRegularTriangulation tri
      Ts/isRegularTriangulation

      regularTriangulationWeights tri
    Text
      The following code determines the support of each triangulation, and tallies them.
      Thus for example, we see that there are 6 regular fine triangulations.
    Example
      tally for tri in Ts list sort unique flatten max tri
    Text
      The method that topcom uses depends on the optional arguments {\tt Fine}, {\tt ConnectedToRegular}
      and {\tt RegularOnly}. 
    Example 
      options allTriangulations
    Text
      If the optional argument {\tt Fine} is set to true, then only 
      {\it fine} triangulations (i.e.
          those that involve every column of $A$) will be generated.
    Example 
      Ts = allTriangulations(A, Fine => true);
      #Ts == 6
    Text
      If the optional argument {\tt RegularOnly} is set to false, but
      {\tt ConnectedToRegular} is true, it will generally take less time,
      as the program doesn't need to check each triangulation to see if it is regular.
    Example
      T1s = allTriangulations(A, RegularOnly => true)
      T2s = allTriangulations(A, RegularOnly => false)
      #T1s
      #T2s
    Text
      The following search would also yield all triangulations, even those not connected via
      bistellar flips to regular triangulations.
    Example
      T3s = allTriangulations(A, RegularOnly => false, ConnectedToRegular => false)
      #T3s
    Text
      Given the list of triangulations, we can query them using other topcom functions.
      See also @TO "Triangulations::Triangulations"@ for other functionality.
    Example
      netList Ts
      for tri in Ts list isWellDefined tri
      for tri in Ts list isRegularTriangulation tri
      for tri in Ts list regularTriangulationWeights tri
  SeeAlso
    (topcomNumTriangulations, Matrix)
    generateTriangulations
///

doc ///
  Key
    generateTriangulations
    (generateTriangulations, Matrix)
    [generateTriangulations, Limit]
    [generateTriangulations, RegularOnly]
    [generateTriangulations, Homogenize]
  Headline
    generate all triangulations with certain properties
  Usage
    Ts = generateTriangulations A
    Ts = generateTriangulations(A, tri)
    Ts = generateTriangulations T
  Inputs
    A:Matrix
      over the integers (or rationals?), whose columns are the
      points to use
    tri:List
      A list of lists of indices representing a triangulation of 
      the columns of $A$
    T:Triangulation
      packages both $A$ and {\tt tri} in one object
    Limit => ZZ
      Stop after constructing this many triangulations
    RegularOnly => Boolean
      Only generate regular triangulations
    Homogenize => Boolean
      set to false in the case the columns form a vector configuration.
      The default case is that the columns of $A$ are considered a point
      configuration (not used in the variant {\tt generateTriangulations T})
  Outputs
    Ts:List
        of lists of integers, each such list represents a triangulation
  Description
    Text
      This function can be used to generate a set of triangulations of a point set 
      or vector configuration $A$ (the points are the columns of $A$).
      
      It operates by starting with one triangulation (tri or T), if one is given, and if 
      not, it constructs a fine triangulation of the set of columns of $A$.
      
      After this, it uses bistellar flips to generate neighbors, and continues, until
      the limit is reached, or no new ones can be constructed.
      
      Important note! This function generally starts with a fine triangulation (i.e. one
      using all of the points in $A$), and only considers bistellar flips that give fine 
      triangulations.
    Example
      A = vertices hypercube 3
      T = topcomRegularFineTriangulation A
      tri = regularFineTriangulation A
      Ts1 = generateTriangulations A -- list of Triangulation's.
      Ts2 = generateTriangulations(A, T) -- list of list of subsets
      Ts3 = generateTriangulations triangulation(A, T) -- list of Triangulations
      Ts4 = generateTriangulations tri -- list of Triangulations
      all(Ts4, isFine)
      all(Ts4, isStar)
      all(Ts4, isRegularTriangulation)
      Ts4/isStar//tally
      Ts4/gkzVector
      volume convexHull A -- 8
      stars1 = select(Ts4, t -> (gkzVector t)#-1 == 8)
      stars2 = select(Ts4, isStar)
      stars1 == stars2
  Caveat
    This function is written in the top level Macaulay2 language, and so is much slower
    than @TO "allTriangulations"@, which calls the topcom code written in C++.  On the other hand,
    one can give this function a limit for the number of triangulations to generate,
    so can be used to generate triangulations in the case when the number is too large to 
    write down all of them.
  SeeAlso
    "allTriangulations"
///

TEST ///
-- of homogenization and need for it.
-*
  restart
  debug needsPackage "Triangulations"
*-
  -- test of isRegularTriangulation
  A = transpose matrix {{-1,-1},{-1,1},{1,-1},{1,1},{0,0}}
  regularFineTriangulation(A, Homogenize=>false) -- returns null.  What does this mean?
  T = regularFineTriangulation(A, Homogenize=>true) -- this is good.
  needsPackage "Polyhedra"
  assert(volume convexHull A == 4) -- actual volume
  assert(sum (for t in max T list volume convexHull A_t) == 4)

  A1 = transpose matrix {{-1,-1,1},{-1,1,1},{1,-1,1},{1,1,1},{0,0,1}}
  T1 = regularFineTriangulation(A1, Homogenize=>false)
  assert(T1 === T)
  assert(max T === {{0, 1, 4}, {0, 2, 4}, {1, 3, 4}, {2, 3, 4}}) -- they are sorted, so it should be this.
  assert isRegularTriangulation T
  regularTriangulationWeights T == {1,1,0,0,0}
///

TEST ///
-*
  restart
  needsPackage "Triangulations"
*-
  A = transpose matrix {{-1,-1},{-1,1},{1,-1},{1,1},{0,0}}
  T = regularFineTriangulation A
  naiveIsTriangulation T -- TODO: doc this, and allow A to be homogenized? Same with topcomIsTriangulation
  -- XX
  assert(set flips T === set{{{0, 3}, {4}}, {{1, 2}, {4}}})
  orientedCircuits A
  assert isSubset(flips T, orientedCircuits A)
  orientedCocircuits A
  chirotope A
  assert(naiveChirotopeString A === chirotopeString A)
  topcomNumTriangulations(A, RegularOnly => false, ConnectedToRegular => false) -- this should really be the default?
  allTriangulations A
///

TEST ///
-- TODO: this is a test for Topcom, it seems?
-*
  restart
  needsPackage "Triangulations"
*-
  -- test of isRegularTriangulation
  A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
  tri = {{0,1,2}, {1,3,5}, {2,3,4},
         {0,1,5}, {0,2,4}, {3,4,5},
         {1,2,3}}
  assert not isRegularTriangulation(A,tri)
  assert(null === regularTriangulationWeights(A,tri))
  topcomNumTriangulations A
  allTriangulations A  
  allTriangulations(A, Fine=>true)
  allTriangulations(A, Fine=>true, RegularOnly=>false)
  A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{7,-2}}
  tri = {{0,1,2}, {1,3,5}, {2,3,4},
         {0,1,5}, {0,2,4}, {3,4,5},
         {1,2,3}}
  assert isRegularTriangulation(A,tri)
  regularTriangulationWeights(A,tri) -- Question: how to test that this is correct
    -- TODO: need a function which takes a point set, weights, and creates the lift (easy)
    --       compute the lower hull of this polytope.

  assert(toString chirotope A === naiveChirotopeString A)
  orientedCircuits A
  orientedCocircuits A
  A = transpose matrix {{1,0},{0,1}}
  tri = {{0,1}}
  assert isRegularTriangulation(A,tri)
  regularTriangulationWeights(A,tri) == {0,1} -- TODO: check that this is the correct answer
  
  A = transpose matrix {{0}}
  tri = {{0}}
  assert isRegularTriangulation(A,tri)
  regularTriangulationWeights(A,tri) == {1}
///

///
-- TODO: This test needs to be made to assert correct statements
-- How to test that triangulations are correct?  What I thought worked does not.
  needsPackage "Triangulations"
  A = transpose matrix{{-1,-1},{-1,1},{1,-1},{1,1},{0,0}}
  regularFineTriangulation A  
  tri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 3, 4}}
  tri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 2, 3}}
  isRegularTriangulation(A, tri) -- Wrong!!

  badtri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 2, 3}}
  debugLevel = 6
  isRegularTriangulation(A,badtri) -- this should fail! But it doesn't seem to do so. BUG in something!!!
  debugLevel = 0
  -- hmmm, we can make non-sensical triangulations, without it noticing.
  -- this should be a bug?  
  A = transpose matrix {{0,0},{0,1},{1,0},{1,1}}
  tri = {{0,1,2},{0,2,3}}
  assert isRegularTriangulation(A,tri)  
  tri = {{0,1,2},{1,2,3}}
  assert isRegularTriangulation(A,tri) 
///

TEST ///  
  needsPackage "Triangulations"
  needsPackage "Polyhedra"
  
  A = transpose matrix {{-1,-1,2},{-1,0,1},{-1,1,1},{0,-1,2},{0,1,1},{1,-1,3},{1,0,-1},{1,1,-2}}
  debugLevel = 0
  T = regularFineTriangulation A
  assert isRegularTriangulation T
  assert(regularTriangulationWeights T =!= null)

  A = transpose matrix {{-1, 0, -1, -1}, {-1, 0, 0, -1}, {-1, 1, 2, -1}, {-1, 1, 2, 0}, {1, -1, -1, -1}, {1, -1, -1, 1}, {1, 0, -1, 2}, {1, 0, 1, 2}}
  P2 = polar convexHull A
  C = matrix {latticePoints P2}
  tri = regularFineTriangulation C
  assert isRegularTriangulation tri
  regularTriangulationWeights tri -- is this correct?  Some weights have negative values??
///


TEST ///
-- simple example of chirotope
-*
  restart
  needsPackage "Triangulations"
*-
  A = transpose matrix {{-1,-1},{-1,1},{1,-1},{1,1},{0,0}}
  tri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 3, 4}}
  ch1 = chirotope A
  ch2 = naiveChirotope A
  assert(ch1 === ch2)
///

TEST ///
-- Bad triangulations of the square
V = transpose matrix {{0,0},{1,0},{0,1},{1,1}}
T1 = {{0,1,2}}
T2 = {{0,1,2},{0,1,3}}
T3 = {{0,1,2,3}}
assert(not naiveIsTriangulation(V, T1))
assert(not naiveIsTriangulation(V, T2))
assert(not naiveIsTriangulation(V, T3))
-- assert(not topcomIsTriangulation(V, T1)) -- topcom signals an error here
-- assert(not topcomIsTriangulation(V, T2)) -- topcom signals an error here
assert(not topcomIsTriangulation(V, T3))
///

-- This example is a good one, but takes too long to be run automatically
-- Actually, this is a test of Topcom....
///
restart
  needsPackage "Topcom"  
  needsPackage "Polyhedra"
  pts =  {{-1,0,0,-1},{-1,0,1,-1},{-1,0,1,0},{-1,1,0,-1},{-1,1,0,0},{-1,1,1,2},{1,-1,0,-1},{1,0,-1,1},{1,-1,-1,-1},{0,0,0,-1}}
  A = transpose matrix pts 

  debugLevel = 7

  elapsedTime n1 = numTriangulations(A, Fine=>true, ConnectedToRegular=>true) -- 6.9 sec, 408 of these CORRECT
  elapsedTime n2 = numTriangulations(A, Fine=>true, ConnectedToRegular=>false, RegularOnly => false) -- 116 sec, 448 of these
  elapsedTime n3 = numTriangulations(A, Fine=>false, ConnectedToRegular=>true)  -- 8 sec, 520 of these CORRECT
  elapsedTime n4 = numTriangulations(A, Fine=>false, ConnectedToRegular=>false, RegularOnly => false) -- 115 sec, 564 of these

  elapsedTime n5 = numTriangulations(A, Fine=>true, ConnectedToRegular=>true, RegularOnly=>false) -- .09 sec, 448 of these
  --this is n2 above.  elapsedTime n6 = numTriangulations(A, Fine=>true, ConnectedToRegular=>false, RegularOnly=>false) -- 115.5 sec, 448 of these
  elapsedTime n7 = numTriangulations(A, Fine=>false, ConnectedToRegular=>true, RegularOnly=>false)  -- .11 sec, 564 of these
  -- this is n4 above. elapsedTime n8 = numTriangulations(A, Fine=>false, ConnectedToRegular=>false, RegularOnly=>false) -- 116 sec, 564 of these

  elapsedTime set1 = allTriangulations(A, Fine=>true, ConnectedToRegular=>true); -- 6.9 sec, 408  CORRECT
  elapsedTime set2 = allTriangulations(A, Fine=>true, ConnectedToRegular=>false, RegularOnly => false); -- 118 sec, 448
  elapsedTime set3 = allTriangulations(A, Fine=>false, ConnectedToRegular=>true); -- 8.1 sec, 520 CORRECT
  elapsedTime set4 = allTriangulations(A, Fine=>false, ConnectedToRegular=>false, RegularOnly => false); -- 116 sec.  564 of these.

  elapsedTime set5 = allTriangulations(A, Fine=>true, ConnectedToRegular=>true, RegularOnly=>false); -- .15 sec, 448 of these
  --elapsedTime set6 = allTriangulations(A, Fine=>true, ConnectedToRegular=>false, RegularOnly=>false); -- 116 sec, 448 of these
  elapsedTime set7 = allTriangulations(A, Fine=>false, ConnectedToRegular=>true, RegularOnly=>false); -- .22 sec, 564 of these
  --elapsedTime set8 = allTriangulations(A, Fine=>false, ConnectedToRegular=>false, RegularOnly=>false); -- 117 sec, 564 of these

  assert((n1,n2,n3,n4,n5,n7) == (#set1, #set2, #set3, #set4, #set5, #set7))
  fineTris = select(set4, x -> # unique flatten x == numColumns A);
  regularFineTris = select(fineTris, x -> isRegularTriangulation(A, x));
  regularTris = select(set4, x -> isRegularTriangulation(A, x));

  assert(#regularFineTris == 408)
  assert(#fineTris == 448)
  assert(#regularTris == 520)  

  assert(set set5 === set set2) -- in general, this doesn't need to hold, but it is rare for this to be the case
  assert(set set7 === set set4) -- same: rare for this to not hold
  elapsedTime assert(set select(set7, x -> isRegularTriangulation(A, x)) === set set3)
  elapsedTime assert(set select(set5, x -> isRegularTriangulation(A, x)) === set set1)

-- the rest of this test needs to be checked, and cleaned up
  set5_0
  elapsedTime for tri in set5 list naiveIsTriangulation(A, tri)

  numFlips(A, set5_0)  
  topcomFlips(A, set5_0)
  -- now let's see about the naive way of getting regular star triangulations 
  -- i.e. we add in the origin
  
  pts1 =  {{-1,0,0,-1},{-1,0,1,-1},{-1,0,1,0},{-1,1,0,-1},{-1,1,0,0},{-1,1,1,2},{1,-1,0,-1},{1,0,-1,1},{1,-1,-1,-1},{0,0,0,-1},{0,0,0,0}}
  A1 = transpose matrix pts1
  --elapsedTime tris1 = allTriangulations(A1, Fine=>true, ConnectedToRegular=>true, RegularOnly=>false); -- 
  elapsedTime tris1 = allTriangulations(A1, Fine=>false, ConnectedToRegular=>true, RegularOnly=>false); -- 
  fineTris1 = select(tris1, x -> # unique flatten x == numColumns A1);
  regTris1 = select(tris1, x -> isRegularTriangulation(A1, x));  
  fineRegTris1 = select(regTris1, x -> # unique flatten x == numColumns A1);
  stars1 = select(tris1, x -> all(x, x1 -> member(10, x1))); -- 100 here
  starsFine1 = select(stars1, x -> # unique flatten x == numColumns A1);
  RST = select(stars1, x -> isRegularTriangulation(A1,x)); -- 80 here...
  FSRT = select(starsFine1, x -> isRegularTriangulation(A1,x)); -- 48 here...!


  unique for tri in set5 list fineStarTriangulation(A, tri);
  --select(oo, tri -> isRegularTriangulation(A1, tri))  -- this isn't defined here...

  -- let's test this one for being a triangulation:
  oA = orientedCircuits A
  tri = set5_3
  tally flatten for t in tri list subsets(t,4)
  for z in oA list (
      # select(tri, t -> isSubset(z_0, t)),
      # select(tri, t -> isSubset(z_1, t))
      )
  -- todo:
  -- 1. routine to check that a triangulation is a triangulation
  -- 2. routine to turn a regular, fine triangulation, into a star (fine, regular) triangulation. How general is this? DONE, I think.
  -- 3. perform bistellar flips to get new triangulations.
///

TEST ///
  restart
  needsPackage "ReflexivePolytopesDB"
  needsPackage "Topcom"  
  needsPackage "Polyhedra"
  debug needsPackage "Triangulations"
  
 str = "4 18  M:53 18 N:11 10 H:6,45 [-78]
        1   0   0  -2   0   2   1   3  -2   2  -2   2   3  -1   0  -2   0  -1
        0   1   0   2   0   0   1  -2   1  -2   0   0  -1   0  -2   0  -2  -1
        0   0   1   1   0  -1  -1  -2   2  -2   0  -2  -2   2  -1   2   1   2
        0   0   0   0   1  -1  -1   0  -1   1   1  -1  -1  -1   2  -1   0  -1"
 A = matrix first kreuzerSkarke str
 P = convexHull A
 P2 = polar P
 A1 = vertices P2
 LP = matrix{select(latticePoints P2, x -> x != 0)}
 numTriangulations(LP, Fine => true) == 731
 elapsedTime tris = allTriangulations(LP, Fine=>true);
 #tris == 731
 numTriangulations(LP)
 allTriangulations(LP);
 
 T = regularFineTriangulation LP
 elapsedTime trisT = generateTriangulations T; -- much slower, and includes 10 non-regular triangulations.
 trisT/(T -> isRegularTriangulation T)//tally

 (24) * volume P2  == 29
 
///

TEST ///
-- this is an example used in 
-- https://people.inf.ethz.ch/fukudak/lect/mssemi/reports/03_rep_ClemensPohle.pdf
-- (accessed 2 June 2022)
-*
  restart
  needsPackage "Triangulations"  
*-
  debug needsPackage "Topcom"  

  -- this tests construction of the chirotope.
  A = transpose matrix"0,0;1,1;3,1;5,0;1,5"
  OM = chirotope A
  -- "foo-input.in" << topcomPoints A << endl << close;
  -- directString = get("!"|(topcompath|"points2chiro <foo-input.in "));
  -- assert(directString == toString chirotope A)
  assert(toString OM == "5,3:\n--+-++-+++\n")

  -- now get one triangulation
  -- TODO: return Triangulation.  sort triangulation.  get weights? (optionally?)
  elapsedTime T = regularFineTriangulation A -- sort the output of this?
  assert((max T)/sort//sort == ({{0, 1, 2}, {0, 2, 3}, {1, 2, 4}, {0, 1, 4}, {2, 3, 4}})/sort//sort)

  -- the above T is the same here as the placing triangulation (not often the same, I think).
  -- elapsedTime tri = value get("!"|(topcompath|"points2placingtriang <foo-input.in "));
  -- assert(tri/sort//sort == ({{0, 1, 2}, {0, 2, 3}, {1, 2, 4}, {0, 1, 4}, {2, 3, 4}})/sort//sort)

  -- now find the possible flips.  Can we get topcom to do the flips?
  assert(set flips T === set {{{0, 2, 4}, {1}}, {{1, 3}, {0, 2}}})
  assert(isSubset((flips T)/sort, (orientedCircuits A)/sort))
  -- I don't see how to get topcom to use these though.
///

TEST ///
-*
  restart
  needsPackage "Triangulations"
*-
  -- test of bistellar flips code.
  
  -- example 1.
  debug needsPackage "Topcom"
  A = transpose matrix"0,0;1,1;3,1;5,0;1,5"
  tri = regularFineTriangulation A
  max tri
  assert(max tri == (max tri)/sort//sort)
  
  flips tri -- TODO: return list of "affine circuits"
  possibles = affineCircuits tri -- TODO: change name.  Have it only return possible flips?
  tri2 = bistellarFlip(tri, possibles#0)
  tri3 = bistellarFlip(tri, possibles#1)
  bistellarFlip(tri, possibles#2) -- null
  bistellarFlip(tri, possibles#3) -- null

  neighbors tri
  nbors = possibles/(c -> c => bistellarFlip(tri, c))
  24 * gkzVector tri
  for t in nbors list if last t === null then continue else {t, gkzVector last t}
  gkzVector tri    
///

TEST /// -- test of functions here on the square and the cube
-*
  restart
  needsPackage "Triangulations"
*-
  square = transpose matrix{{1,1},{-1,1},{-1,-1},{1,-1}}
  
  topcomRegularFineTriangulation square
  assert(# allTriangulations square == 2)
    
  -- Now consider all of the lattice points of the square
  sq9 = matrix {{-1, -1, 1, 1, -1, 0, 0, 1, 0}, {-1, 1, -1, 1, 0, -1, 1, 0, 0}}

  -- test function from Topcom.
  t1 = topcomRegularFineTriangulation sq9
  regularTriangulationWeights(sq9, t1)
  fineStarTriangulation(sq9, t1) -- has central element removed.  Don't do that?
  delaunaySubdivision sq9 -- not a triangulation (4 squares).
  orientedCircuits sq9 -- many of these are not useful when considering only fine triangulations.

  -- Polyhedra command, bug fix currently in StringTorics.
  t2 = regularSubdivision(sq9, matrix{{4,4,4,4,1,1,1,1,0}})
  assert(
    t2 
    == 
    {{0, 4, 5}, {1, 4, 6}, {2, 5, 7}, {3, 6, 7}, {4, 5, 8}, {4, 6, 8}, {5, 7, 8}, {6, 7, 8}}
    )

  t3 = regularSubdivision(sq9, matrix{{4,4,4,4,1,1,1,1,-4}})
  t4 = regularSubdivision(sq9, matrix{{1,1,1,1,1,1,1,1,-4}})
  
  affineCircuits(sq9, t3)
  elapsedTime affineCircuits(sq9, t1) -- TODO: change the name of this function? Why? affineOrientedCircuits?
  
  elapsedTime tris = allTriangulations sq9; -- computes all triangulations, Fine => false, regular.
  assert(387 == #tris) -- check this number. 
  elapsedTime tris1 = allTriangulations(sq9, Fine => true);
  assert(64 == #tris1)
  elapsedTime tris2 = generateTriangulations triangulation(sq9, t1);
  elapsedTime tris2a = generateTriangulations(triangulation(sq9, t1), RegularOnly => true); -- slow...
  #tris2 == #tris2a

  -- via topcom: (too long for general test)
  ----elapsedTime assert(387 == # select(tris, t -> topcomIsRegularTriangulation(sq9, max t))) -- slow
  -- via Polyhedra/ (too long for a general test)
  ----elapsedTime assert(387 == # select(tris, t -> null =!= regularTriangulationWeights t)) -- slow, same as Polyhedra, I think (same code is being used)
  -- 64 fine triangulations
  assert(64 == # select(tris, isFine))
  assert(16 == # select(tris, isStar))
  assert(1 == # select(tris, t -> isStar t and isFine t))

  -- generate only fine 
  elapsedTime finetris = allTriangulations(sq9, Fine => true);
  -- elapsedTime finetris1 = generateTriangulations(sq9, t1, Fine => true); -- TODO
  elapsedTime finetris1 = generateTriangulations triangulation(sq9, t1);
  assert(finetris/max//sort == finetris1/max//sort)

  finetris/volumeVector
  
  -- TODO: audit/allow "A" matrices to be homogeneous or not?  THIS MIGHT BE DONE...
  --   one way: default is not, and Homogenize => true will add the extra row.
  --   use this for most routines taking point configurations or vector configurations.
///

TEST ///
  -- triangulation code, test 1.  
  -- some triangulation code is in Topcom, Polyhedra
  -- (comes from KS database, h11=3, #232.
-*
  restart
  needsPackage "Triangulations"
*-  

  A0 = matrix {{1, 1, 1, 1, -11}, {0, 2, 2, 2, -10}, {0, 0, 4, 4, -8}, {0, 0, 0, 12, -12}}
  P2 = polar convexHull A0
  -- the floowing are the lattice points of P2 (from 'LP = latticePointList P2', but this uses StringTorics...)



  LP = {{-1, 0, 0, 0}, {-1, 0, 0, 1}, {-1, 0, 3, -1}, {-1, 2, -1, 0}, {1, -1, 0, 0}, {-1, 0, 1, 0}, {-1, 1, 0, 0}, {0, 0, 0, 0}}  
  A  = transpose matrix LP
  -- We want triangulations of the point configuration given by the columns of A.
  T = topcomRegularFineTriangulation A
  assert isFine(A, T)
  assert isStar(A, T)
  assert isRegularTriangulation(A, T)

  assert naiveIsTriangulation(A, T)      
  assert topcomIsTriangulation(A, T)      

  elapsedTime tris = allTriangulations A;
  FRST = first select(tris, t -> isFine t and isStar t and isRegularTriangulation t)
  volumeVector FRST
  elapsedTime tris = generateTriangulations(A, T);

  wts = regularTriangulationWeights(A, T)
  assert(regularSubdivision(A, matrix{wts}) == T//sort)
  assert isRegularTriangulation(A, T) -- from Topcom.
  assert topcomIsTriangulation(A, T)

  -- also check Topcom's code...
  neighbors triangulation(A, T) -- not unpacked... TODO: need to understand and document what this gives

  --A1 = A || splice matrix{{numColumns A: 1}}
  --A1 = augment A
  A1 = A || matrix{{numcols A: 1}}
  --volumeVector(A, T) -- need to homogenize A? at bottom or at top?
  volumeVector(A1, T) -- need to homogenize A? at bottom or at top?
  neighbors triangulation(A1, T)
  --regularFineStarTriangulation A1 -- this one fails
  regularFineStarTriangulation A -- this one works
  
  -- check that T is a triangulation?
  circs = affineCircuits(A, T) -- NEEDS CLEANING UP (? why?)
  bistellarFlip(T, {{5}, {0, 1, 2}})  
  for c in circs list bistellarFlip(T, c)
///



-- The following is too long for a test.
///
  -- how many triangulations at h11=8? (roughly?)
  needsPackage "ReflexivePolytopesDB"  
  topes = kreuzerSkarke 8;
  A = matrix topes_500
  P = convexHull A
  P2 = polar P
  LP = matrix{latticePoints P2}
  T = regularFineTriangulation LP

  T = {T}
  T1 = T/neighbors/(t -> t/last)//flatten
  T1/gkzVector

  #T1
  T2 = T1/(t -> neighbors t)//flatten/last//unique;
  T2/gkzVector//matrix
  T1 = T2;
  


    
  methods allTriangulations
  options allTriangulations
  debugLevel = 1
  elapsedTime trisLP = allTriangulations(LP, Fine => true, RegularOnly => true);  
  frsts = for t in trisLP list if all(t, t1 -> member(numcols LP - 1, t1)) then t else continue;
  #frsts == 108
  isStar(t, numcols LP - 1) then t else continue;
  T = regularFineTriangulation LP
  debug Triangulations
  debugLevel = 0
  elapsedTime generateTriangulations(T, Limit => 20000);
///

end----------------------------------------------------

restart
uninstallPackage "Triangulations"
restart
needsPackage "Triangulations"
restart
installPackage "Triangulations"
restart
check "Triangulations"

TEST /// 
  debug needsPackage "Topcom"
  -- points2chiro
  toppath = "/opt/homebrew/bin/"
  A = transpose matrix {{-1,-1,1},{-1,1,1},{1,-1,1},{1,1,1},{0,0,1}}
  tri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 3, 4}}
  run (toppath|"/points2chiro"|" -h")
  "topcomfoo.in" << topcomPoints(A, Homogenize=>false) << endl << close;
  chiro = get ("!"|toppath|"/points2chiro"|" <topcomfoo.in")
  #chiro
  chiro2 = "5,3:\n" | (concatenate for s in sort subsets(5,3) list (
      d := det A_s;
      if d > 0 then "+" else if d == 0 then "0" else "-"
      )) | "\n"
  chiro == chiro2
  -- notes: a chirotope for topcom:
  --  5,3:  (number of vertices, dim)
  --  a string of "-","+","0", maybe cut over a number of lines.
  -- should we make a type out of this (so we can read and write it to a file)

  -- chiro2circuits
  "topcomfoo.in" << chiro << endl << [] << endl << close;
  circs = get ("!"|toppath|"/chiro2circuits"|"  <topcomfoo.in")
  cocircs = get ("!"|toppath|"/chiro2cocircuits"|"  <topcomfoo.in")
  drop(drop(circs, 2), -1)
  oo/value

chiro = "5, 3:"

r12'chiro = "12, 4:
-+--+++---++---++-+---++-+++-----++--++-++++--++---+++-+++--+---++---++--++-++++
--+---++-+++--+++--++--+----+-+++--+++--++--+----+---++--++-++++-++--+-----+----
-++---++---+++-+++--+---++---++--++-++++--+---++-+++--+++--++--+----+-+++--+++--
++--+----+---++--++-++++---++-+++++-+++++--++++---++-+++--+++--++--+----+-+++--+
++--++--+----+---++--++-++++---++-+++++-+++++--+++---++---++--++-++++-+++--++--+
----+++--+-----+-----++--+++--++--+----+++--+-----+-----++----++-+++++-+++++--++
--------++--++-
"
  chiro = r12'chiro
  -- chiro2alltriangs, chiro2nalltriangs
  "topcomfoo.in" << "5, 3:" << endl << chiro << endl << [] << endl << close;
  "topcomfoo.in" << chiro << [] << endl << close;
  get ("!"|toppath|"/chiro2placingtriang"|" -v <topcomfoo.in")
  get ("!"|toppath|"/chiro2circuits"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2cocircuits"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2alltriangs"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2ntriangs"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2finetriang"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2finetriangs"|" <topcomfoo.in") -- what is the format of the output here??
  get ("!"|toppath|"/chiro2nfinetriangs"|" -v <topcomfoo.in")
  
///

TEST ///
  restart
  debug needsPackage "Topcom"
  needsPackage "ReflexivePolytopesDB"
  needsPackage "StringTorics"
  polytopes = kreuzerSkarke(50, Limit=>10);
  tope = polytopes_5
  A = matrix tope
  P = convexHull A
  P2 = polar P
  A = matrix{latticePoints P2}

  LP = drop(latticePointList P2, -1);
  A = transpose matrix LP;
  debugLevel = 6
  elapsedTime tri = regularFineTriangulation A;
  
  -- XXX
  augment A
  "topcomfoo.in" << topcomPoints(augment A, Homogenize=>false) << endl << close;
  chiro = get ("!"|toppath|"/points2chiro"|" <topcomfoo.in")

  "topcomfoo.in" << chiro << "[]" << endl << close;
  get ("!"|toppath|"/chiro2circuits"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2ntriangs"|" <topcomfoo.in")
  --get ("!"|toppath|"/chiro2alltriangs"|"  <topcomfoo.in")
  get ("!"|toppath|"/chiro2cocircuits"|" <topcomfoo.in")    
///

TEST ///
-- how to check a triangulation?  I don't think that Topcom has this implemented for general use.
-*
  restart
  debug needsPackage "Topcom"
*-
  -- test of isRegularTriangulation
  toppath = "/Users/mike/src/M2-master/M2/BUILD/dan/builds.tmp/as-mth-indigo.local-master/libraries/topcom/build/topcom-0.17.8/src/"
  A = transpose matrix {{-1,-1,1},{-1,1,1},{1,-1,1},{1,1,1},{0,0,1}}
  badtri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 2}}
  debugLevel = 6

  -- a regular triangulation
  A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{7,-2}}
  tri = {{0,1,2}, {1,3,5}, {2,3,4},
         {0,1,5}, {0,2,4}, {3,4,5},
         {1,2,3}}
  "topcomfoo.in" << topcomPoints(A, Homogenize=>true) << endl << "[]" << endl << tri << endl << close;
  run (topcompath|"checkregularity"|" --heights <topcomfoo.in >topcomfoo.out")  

  -- points2chiro
  "topcomfoo.in" << topcomPoints(A, Homogenize=>false) << endl << "[]" << endl << badtri << endl << close;
  print (toppath|"/points2alltriangs"|" --checktriang -v <topcomfoo.in") 


  A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
  tri = {{0,1,2}, {1,3,5}, {2,3,4},
         {0,1,5}, {0,2,4}, {3,4,5},
         {1,2,3}}
  "topcomfoo.in" << topcomPoints(A, Homogenize=>true) << endl << "[]" << endl << tri << endl << close;
  run (topcompath|"checkregularity"|" --heights <topcomfoo.in >topcomfoo.out")
  assert not isRegularTriangulation(A,tri)

///

///  
  -- now let's see about the naive way of getting regular star triangulations 
  -- i.e. we add in the origin
  
  pts1 =  {{-1,0,0,-1},{-1,0,1,-1},{-1,0,1,0},{-1,1,0,-1},{-1,1,0,0},{-1,1,1,2},{1,-1,0,-1},{1,0,-1,1},{1,-1,-1,-1},{0,0,0,-1},{0,0,0,0}}
  A1 = transpose matrix pts1
  elapsedTime tris1 = allTriangulations(A1, Fine=>false, ConnectedToRegular=>true, RegularOnly=>false); -- 
  fineTris1 = select(tris1, x -> # unique flatten x == numColumns A1);
  regTris1 = select(tris1, x -> isRegularTriangulation(A1, x));  
  fineRegTris1 = select(regTris1, x -> # unique flatten x == numColumns A1);
  stars1 = select(tris1, x -> all(x, x1 -> member(10, x1))); -- 100 here
  starsFine1 = select(stars1, x -> # unique flatten x == numColumns A1);
  RST = select(stars1, x -> isRegularTriangulation(A1,x)); -- 80 here...
  FSRT = select(starsFine1, x -> isRegularTriangulation(A1,x)); -- 48 here...!
  #tris1 == 2254
  #RST == 80
  #FRST = 48

  unique for tri in set5 list (
      tri1 := fineStarTriangulation(A, tri);
      newtri := for t in tri1 list append(t, 10);
      newtri
      );
  select(oo, tri -> isRegularTriangulation(A1, tri))  

  -- let's test this one for being a triangulation:
  oA = orientedCircuits A
  tri = set5_3
  tally flatten for t in tri list subsets(t,4)
  for z in oA list (
      # select(tri, t -> isSubset(z_0, t)),
      # select(tri, t -> isSubset(z_1, t))
      )
///

TEST ///
-- Bad triangulations of the square
  V = transpose matrix {{0,0},{1,0},{0,1},{1,1}}
  T1 = {{0,1,2}}
  T2 = {{0,1,2},{0,1,3}}
  T3 = {{0,1,2,3}}
  assert(not topcomIsTriangulation(V, T1))
  assert(not topcomIsTriangulation(V, T2))
  assert(not topcomIsTriangulation(V, T3))

  debug needsPackage "Triangulations" -- TODO: isTriangulation should be exported, or called naiveIsTriangulation.
  assert(not isTriangulation(V, T1))
  assert(not isTriangulation(V, T2))
  assert(not isTriangulation(V, T3)) -- gives error, should give false!
///


end--

restart
uninstallPackage "Topcom"
restart
needsPackage "Topcom"
installPackage "Topcom"
restart
check "Topcom"
viewHelp

///
-- generate examples to use for this package
-- from reflexive polytopes of dim 4
  restart
  needsPackage "StringTorics"

  str = getKreuzerSkarke(10, Limit=>5)
  str = getKreuzerSkarke(20, Limit=>5)
  str = getKreuzerSkarke(30, Limit=>5)
  polytopes = parseKS str
  tope = polytopes_4_1
  A = matrixFromString tope
  P = convexHull A
  P2 = polar P
  LP = drop(latticePointList P2, -1)
  A1 = transpose matrix LP
  A2 = transpose matrix latticePointList P2
  tri = regularFineTriangulation A1
  tri2 = regularFineTriangulation A2
  #tri
  #tri2
  elapsedTime chiro1 = chirotope A1;
  elapsedTime chiro2 = chirotope A2;
  elapsedTime # orientedCircuits chiro1
  elapsedTime # orientedCircuits chiro2
  elapsedTime # orientedCocircuits chiro1
  elapsedTime # orientedCocircuits chiro2
  (select(orientedCocircuits A2, f -> #f#0 == 0 or #f#1 == 0))/first
  netList annotatedFaces P2  
  tri2
  -- fine:
  assert(sort unique flatten tri2 == toList (0..14))
  walls = tri2/(x -> subsets(x, #x-1))//flatten
  nfacets = tally walls
  facs = (select((annotatedFaces P2), x -> x_0 == 3))/(x -> x#2)
  walls = partition(k -> nfacets#k, keys nfacets)
  for w in walls#1 list (
      # select(facs, f -> isSubset(w, f))
      )
  for w in walls#2 list (
      # select(facs, f -> isSubset(w, f))
      )
  -- check overlaps of elements of tri2:
  C = orientedCircuits A2;
  elapsedTime for c in C list (
      val1 := select(tri2, x -> isSubset(c_0, x));
      val2 := select(tri2, x -> isSubset(c_1, x));
      if #val1 > 0 and #val2 > 0 then print (c, val1, val2);
      (c, #val1, #val2));
  
  tri_0
  
///
  
doc ///
  Key
  Headline
  Usage
  Inputs
  Outputs
  Consequences
  Description
    Text
    Example
  Caveat
  SeeAlso
///

