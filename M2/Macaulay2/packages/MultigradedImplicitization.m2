newPackage(
  "MultigradedImplicitization",
  Version => "1.1",
  Date => "May 15, 2025",
  Authors => {
    {Name => "Joseph Cummings",
    Email => "josephcummings03@gmail.com",
    HomePage => "https://sites.google.com/view/josephcummingsuky/home"},
    {Name => "Benjamin Hollering",
    Email => "benhollering@gmail.com",
    HomePage => "https://sites.google.com/view/benhollering"},
    {Name => "Mahrud Sayrafi",
    Email => "mahrud@umn.edu",
    HomePage => "https://math.umn.edu/~mahrud"}
  },
  Headline => "solving implicitization problems using multigradings",
  Keywords => {"Algebraic Statistics", "Commutative Algebra"},
  PackageImports => {"gfanInterface"}
)

--------------------
--Exports
--------------------

export {
  -- Methods
  "maxGrading",
  "trimBasisInDegree",
  "computeComponent",
  "interpolateComponent",
  "componentsOfKernel",
  -- Options
  "Grading", "PreviousGens", "ReturnTargetGrading", "UseMatroid", "UseInterpolation",
}



---------------------
---- maxGrading -----
---------------------
maxGrading = method(Options => {ReturnTargetGrading => false});
maxGrading RingMap := Matrix => opts -> F -> (

  dom := source F;
  codom := target F;
  elimRing := dom ** codom;
  X := vars dom;
  n := numgens dom;
  elimIdeal := ideal(sub(X, elimRing) - sub(F(X), elimRing));
  
  if opts.ReturnTargetGrading then (transpose linealitySpace(gfanHomogeneitySpace(elimIdeal))) else (transpose linealitySpace(gfanHomogeneitySpace(elimIdeal)))_(toList(0..n-1))
)


TEST ///
A = matrix {{1,1,1,0,0,0,0,0,0}, {0,0,0,1,1,1,0,0,0}, {0,0,0,0,0,0,1,1,1}, {1,0,0,1,0,0,1,0,0}, {0,1,0,0,1,0,0,1,0}};
R = QQ[x_1..x_(numcols A)];
S = QQ[t_1..t_(numrows A)];
F = map(S, R, apply(numcols(A), i -> S_(flatten entries A_i)));
assert(ker(A) == ker(maxGrading(F)));
///



----------------------------
----- trimBasisInDegree ----
----------------------------
trimBasisInDegree = method();
trimBasisInDegree (List, Ring,       HashTable) := Matrix => (deg, dom,    basisHash) -> basisHash#deg
trimBasisInDegree (List, Ring, List, HashTable) := Matrix => (deg, dom, G, basisHash) -> (

  if #G == 0 then (
      return basisHash#deg;
  );

  -- otherwise, we shift G in all possible ways to land in R_deg
  --G = apply(G, g -> sub(g, dom));

  L := apply(G, g -> (
          checkDegree := deg - degree(sub(g, dom));
          if basisHash#?checkDegree then (
              g*basisHash#checkDegree
          ) else (
              -- this else condition is only hit when basis(checkDegree,dom) = |0|
              matrix {{0_(ring G_0)}}
          )
      )
  );
  
  -- stick em all in a matrix
  mat := L#0;
  scan(1..#L-1, i -> mat = mat | L#i);

  -- and collect coefficients.
  (mons, coeffs) := coefficients(mat);

  -- find the independent linear relations
  coeffs = mingens(image sub(coeffs, coefficientRing(dom)));

  -- remove monomials corresponding to pivots
  badMonomials := apply(pivots coeffs, i -> mons_(0,i#0));
  monomialBasis := flatten entries basisHash#deg;
  scan(badMonomials, m -> monomialBasis = delete(m, monomialBasis));

  matrix{monomialBasis}
)


TEST ///
A = matrix {{1,1,1,0,0,0,0,0,0}, {0,0,0,1,1,1,0,0,0}, {0,0,0,0,0,0,1,1,1}, {1,0,0,1,0,0,1,0,0}, {0,1,0,0,1,0,0,1,0}};
R = QQ[x_1..x_(numcols A)];
S = QQ[t_1..t_(numrows A)];
F = map(S, R, apply(numcols(A), i -> S_(flatten entries A_i)));
dom = newRing(R, Degrees => A);
use R;
B = basis(1, source F) | basis(2, source F) | basis(3, source F);
lats = unique apply(flatten entries B, i -> degree(sub(i, dom)));
basisHash = hashTable apply(lats, deg -> {deg, sub(basis(deg, dom), R)});
G = {x_2*x_4-x_1*x_5, x_3*x_4-x_1*x_6, x_3*x_5-x_2*x_6}
assert(trimBasisInDegree({2,1,0,1,1},  dom, G, basisHash) == matrix {{x_2*x_3*x_4}});
///



----------------------------
----- computeComponent ----
----------------------------
computeComponent = method(Options => {PreviousGens => {}});
computeComponent (List, Ring, RingMap, Matrix) := List => opts -> (deg, dom, F, monomialBasis) -> (

  -- collect coefficients into a matrix
  (mons, coeffs) := coefficients(F(sub(monomialBasis, source F)));

  -- find the linear relations among coefficients
  K := gens ker sub(coeffs, coefficientRing(dom));

  newGens := flatten entries (monomialBasis * K);

  newGens
)


computeComponent (List, Ring, RingMap, HashTable) := List => opts ->  (deg, dom, F, basisHash) -> (

  monomialBasis := if basisHash#?deg then basisHash#deg else trimBasisInDegree(deg, dom, opts.PreviousGens, basisHash);

  computeComponent(deg, dom, F, monomialBasis)   
)


computeComponent (List, Ring, RingMap) := List => opts -> (deg, dom, F) -> (

  monomialBasis := basis(deg, dom);

  computeComponent(deg, dom, F, monomialBasis)
)


TEST ///
A = matrix {{1,1,1,0,0,0,0,0,0}, {0,0,0,1,1,1,0,0,0}, {0,0,0,0,0,0,1,1,1}, {1,0,0,1,0,0,1,0,0}, {0,1,0,0,1,0,0,1,0}};
R = QQ[x_1..x_(numcols A)];
S = QQ[t_1..t_(numrows A)];
F = map(S, R, apply(numcols(A), i -> S_(flatten entries A_i)));
dom = newRing(R, Degrees => A);
assert(computeComponent({1,1,0,1,1}, dom, F, matrix {{x_1*x_5, x_2*x_4}}) == {x_2*x_4-x_1*x_5});
///



-------------------------------
----- interpolateComponent ----
-------------------------------
interpolateComponent = method(Options => {PreviousGens => {}});
interpolateComponent (List, Matrix) := List => opts -> (samplePoints, monomialBasis) -> (

  -- collect coefficients into a matrix
  evalBasis := matrix for i from 0 to numcols(monomialBasis)-1 list flatten entries sub(monomialBasis, samplePoints_i);

  -- find the linear relations among coefficients
  K := gens ker evalBasis;

  newGens := flatten entries (monomialBasis * K);

  newGens
)


interpolateComponent (List, Ring, List, HashTable) := List => opts ->  (deg, dom, samplePoints, basisHash) -> (

  monomialBasis := if basisHash#?deg then basisHash#deg else trimBasisInDegree(deg, dom, opts.PreviousGens, basisHash);

  interpolateComponent(samplePoints, monomialBasis)   
)


interpolateComponent (List, Ring, RingMap) := List => opts -> (deg, dom, F) -> (

  monomialBasis := basis(deg, dom);
  KK := coefficientRing(dom);

  samplePoints := for l from 0 to numcols(monomialBasis)-1 list(

    paramVals := apply(gens target F, t -> t => random(KK));

    apply(gens source F, x -> sub(x, dom) => sub(F(x), paramVals))
  );

  interpolateComponent(samplePoints, monomialBasis)
)


TEST ///
A = matrix {{1,1,1,0,0,0,0,0,0}, {0,0,0,1,1,1,0,0,0}, {0,0,0,0,0,0,1,1,1}, {1,0,0,1,0,0,1,0,0}, {0,1,0,0,1,0,0,1,0}};
R = QQ[x_1..x_(numcols A)];
S = QQ[t_1..t_(numrows A)];
F = map(S, R, apply(numcols(A), i -> S_(flatten entries A_i)));
dom = newRing(R, Degrees => A);
assert(interpolateComponent({1,1,0,1,1}, dom, F) == {x_2*x_4-x_1*x_5});
///

exponentMatrix = B -> matrix apply(#B, c -> first exponents B_c)

-----------------------------
----- componentsOfKernel ----
-----------------------------
componentsOfKernel = method(Options => {Grading => null, UseMatroid => true, UseInterpolation => false, CoefficientRing => ZZ/32003, Verbose => true});
componentsOfKernel (Number, RingMap) := MutableHashTable => opts -> (d, F) -> (
  S := source F;
  R := target F;

  print("warning: computation begun over finite field. resulting polynomials may not lie in the ideal");

  A := if opts.Grading === null then maxGrading(F) else opts.Grading;
  KK := opts.CoefficientRing;
  dom := newRing(S, Degrees => A);
  toGradedRing := map(dom, S, gens dom);
  basisHash := new MutableHashTable;
  gensHash := new MutableHashTable;
  G := new MutableList;
  newG := new MutableList;

  if (transpose(matrix {toList(numColumns(A) : 1/1)}) % image(transpose sub(A,QQ))) != 0 then (
    print("ERROR: The multigrading does not refine total degree. Try homogenizing or a user-defined multigrading");
    return;
  );

  -- compute the jacobian of F and substitute in random parameter values in a large finite field
  if opts.UseMatroid then(

    J := jacobian matrix F;
    J = sub(J, apply(gens R, t -> t => random(KK)));
  );
  
  -- initialize list of sample points and boolean for tracking if there are linear relations in the kernel
  areThereLinearRelations := false;
  samplePoints := {};
  
  -- assumes homogeneous with normal Z-grading
  for i in 1..d do elapsedTime (

    if i == 2 and areThereLinearRelations then print("WARNING: There are linear relations. You may want to reduce the number of variables to speed up the computation.");
    if opts.Verbose then print(concatenate("computing total degree: ", toString(i)));

    -- compute monomial bases of all homogeneous components in total degree i
    -- TODO: compute basis in S/G instead, which eliminates trimIdealInDegree
    B := first entries basis(i, S);
    -- multidegrees of the basis elements given degrees A
    lats := entries(exponentMatrix B * transpose A); -- ~50% of time in Sashimi
    -- splits columns of B into buckets with the same multidegree
    -- this could probably be done better but works for now
    splitHash := hashTable(join, apply(#B, c -> (lats#c, {c})));
    -- TODO: submatrix(B, , cols) is slower than matrix{(first entries B)_cols}
    newBasisHash := applyValues(splitHash, cols -> matrix{B_cols});
    basisHash = merge(basisHash, newBasisHash, (i, j) -> j);

    if opts.UseInterpolation then maxBasisSize := max(apply(values(basisHash), k -> numcols(k)));
    
    if opts.Verbose then print(concatenate("number of monomials = ", toString(#B)));
    if opts.Verbose then print(concatenate("number of distinct multidegrees = ", toString(#keys(newBasisHash))));
    if opts.Verbose and opts.UseInterpolation then print(concatenate("sampling ", toString(maxBasisSize), " points from the variety"));

    -- sample additional points from the variety if necessary
    if opts.UseInterpolation and #samplePoints <  maxBasisSize then(

      newPoints := for l from 0 to (maxBasisSize - #samplePoints - 1) list(

        paramVals := apply(gens R, t -> t => random(KK));
        
        apply(gens S, x -> sub(x, dom) => sub(F(x), paramVals))
      );

      samplePoints = samplePoints | newPoints;
    );

    -- this loop can be done completely in parallel
    for deg in keys(newBasisHash) do (

      -- find the indices of support variables of basisHash#deg
      supp := apply(support basisHash#deg, index);

      if (numcols(basisHash#deg) == 1) and (i > 1) then(

        gensHash#deg = {};
        continue;
      );

      if (numcols(basisHash#deg) == 0) then error "basis has no monomials";

      if opts.UseMatroid then(

        if rank(J_supp) == #supp then(

          gensHash#deg = {};
          continue;
        );
      );

      
      -- trim the current monomial basis so we only compute minimal generators
      monomialBasis := trimBasisInDegree(deg, dom, toList G, basisHash);

      -- compute minimal generators using either interpolation or symbolic evaluation of the monomials under F
      gensHash#deg = if opts.UseInterpolation then interpolateComponent(samplePoints, monomialBasis) else computeComponent(deg, dom, F, monomialBasis);
      -- append new generators to G
      scan(gensHash#deg, g -> newG##newG = g);

      -- check if there are linear relations. if so then one can reduce the number of variables
      if i == 1 and #(gensHash#deg) > 0 then (
        areThereLinearRelations = true;
      );
    );
    scan(newG, g -> G##G = g);
    newG = new MutableList from {};
  );
  
  gensHash
)


-- symbolic test 
TEST ///
A = matrix {{1,1,1,0,0,0,0,0,0}, {0,0,0,1,1,1,0,0,0}, {0,0,0,0,0,0,1,1,1}, {1,0,0,1,0,0,1,0,0}, {0,1,0,0,1,0,0,1,0}};
R = QQ[x_1..x_(numcols A)];
S = QQ[t_1..t_(numrows A)];
F = map(S, R, apply(numcols(A), i -> S_(flatten entries A_i)));
dom = newRing(R, Degrees => A);
G = componentsOfKernel(2,F);
G = new HashTable from G;
G = delete(null, flatten values(G));
assert(sub(ideal(G),R) == ker F)
///


-- probabilistic test
TEST ///
A = matrix {{1,1,1,0,0,0,0,0,0}, {0,0,0,1,1,1,0,0,0}, {0,0,0,0,0,0,1,1,1}, {1,0,0,1,0,0,1,0,0}, {0,1,0,0,1,0,0,1,0}};
KK = ZZ/nextPrime(100000)
R = KK[x_1..x_(numcols A)];
S = KK[t_1..t_(numrows A)];
F = map(S, R, apply(numcols(A), i -> S_(flatten entries A_i)));
dom = newRing(R, Degrees => A);
use R;
samplePoints = {{x_1 => -6002, x_2 => -37732, x_3 => -43250, x_4 => 32043, x_5 => -32855, x_6 => -9794, x_7 => 15988, x_8 => 12703, x_9 => 46296}, {x_1 => -44235, x_2 => 32764, x_3 => -554, x_4 => -2799, x_5 => -45090, x_6 => -47890, x_7 => 7916, x_8 => -47725, x_9 => 17466}}
G = interpolateComponent(samplePoints, sub(basis({0, 1, 1, 0, 1}, dom), R))
assert(G == {x_6*x_8-x_5*x_9})
///

-----------------------------
----- Documentation ---------
-----------------------------
beginDocumentation()
-- template for function documentation
--doc ///
--Key
--Headline
--Usage
--Inputs
--Outputs
--Consequences
--  Item
--Description
--  Text
--  Code
--  Pre
--  Example
--  CannedExample
--Subnodes
--Caveat
--SeeAlso
--///

doc ///
Key
  MultigradedImplicitization
Headline
  Package for levaraging multigradings to solve implicitization problems
Description
  Text
    The MultigradedImplicitization package provides methods for computing the maximal $\mathbb{Z}^k$ grading in which the 
    kernel of a polynomial map $F$ is homogeneous and exploiting it to find generators of $\ker(F)$. This package is
    particularly useful for problems from algebraic statistics which often involve highly structured maps $F$ which are
    often naturally homogeneous in a larger multigrading than the standard $\mathbb{Z}$-multigrading given by total degree.
    For more information on this approach see the following:
  Text
    References:

    [1] Cummings, J., & Hollering , B. (2023). Computing Implicitizations of Multi-Graded Polynomial Maps. arXiv preprint arXiv:2311.07678.

    [2] Cummings, J., & Hauenstein, J. (2023). Multi-graded Macaulay Dual Spaces. arXiv preprint arXiv:2310.11587.

    [3] Cummings, J., Hollering, B., & Manon, C. (2024). Invariants for level-1 phylogenetic networks under the cavendar-farris-neyman model. Advances in Applied Mathematics, 153, 102633.
///


doc ///
Key
  maxGrading
  (maxGrading, RingMap)
Headline
  computes the maximal $\mathbb{Z}^k$ grading such that $\ker(F)$ is homogeneous
Usage
  maxGrading(F)
Inputs
  F:RingMap
  ReturnTargetGrading => Boolean
    a boolean which encodes whether or not to also return the grading on {\tt target(F)} which induces the grading on $\ker(F)$
Outputs
  :Matrix
    the maximal $\mathbb{Z}^k$ grading in which $\ker(F)$ is homogeneous
--Consequences
--  asd
Description
  Text
    Computes the maximal $\mathbb{Z}^k$ grading such that the $\ker(F)$ is homogeneous. 
    The columns of the output matrix are the degrees of the corresponding variables in {\tt source(F)}.
    For example, the snippet below shows that the maximal grading of a toric ideal
    is exactly the integer matrix which encodes the monomial map parameterizing the ideal.
  Example
    A = matrix {{1,1,1,0,0,0}, {0,0,0,1,1,1}, {1,0,0,1,0,0}, {0,1,0,0,1,0}, {0,0,1,0,0,1}}
    R = QQ[x_(1,1)..x_(2,3)];
    S = QQ[t_1..t_2, s_1..s_3];
    F = map(S, R, {t_1*s_1, t_1*s_2, t_1*s_3, t_2*s_1, t_2*s_2, t_2*s_3})
    maxGrading(F)
  Text
    The option {\tt ReturnTargetGrading} returns a matrix which also gives the corresponding
    grading on the target ring of $F$ which induces the grading on $\ker(F)$. This option is {\tt false}
    by default. 
  Example
    maxGrading(F, ReturnTargetGrading => true)
///


doc ///
Key
  trimBasisInDegree
  (trimBasisInDegree, List, Ring, List, HashTable)
  (trimBasisInDegree, List, Ring,       HashTable)
Headline
  Finds a basis for the homogeneous component of a graded ring but removes basis elements which correspond to previously computed generators. 
Usage
  trimBasisInDegree(deg, dom, G, B)
  trimBasisInDegree(deg, dom, B)
Inputs
  deg:List
    the degree of the homogeneous component to compute
  dom:Ring
    a graded ring which is the source of a homogeneous ring map $F$
  G:List
    a list of previously computed generators of $\ker(F)$
  B:HashTable
    a hashtable which contains all bases of homogeneous components which correspond to lower total degrees than {\tt deg}
Outputs
  :Matrix
    A monomial basis for the homogeneous component of degree {\tt deg} of {\tt dom} with any monomials which cannot be involved in new generators of $\ker(F)$ removed.  
--Consequences
--  asd
Description
  Text
    Computes a monomial basis for the homogeneous component of degree {\tt deg} of the graded ring {\tt dom} which is the source of a ring map $F$. 
    Monomials which correspond to previously computed relations which are in {\tt G} are automatically removed since they will not yield new generators
    in $\ker(F)$ when applying @TO2{computeComponent, "computeComponent"}@ to this basis.  
  Example
    A = matrix {{1,1,1,0,0,0,0,0,0}, {0,0,0,1,1,1,0,0,0}, {0,0,0,0,0,0,1,1,1}, {1,0,0,1,0,0,1,0,0}, {0,1,0,0,1,0,0,1,0}};
    R = QQ[x_1..x_(numcols A)];
    dom = newRing(R, Degrees => A);
    use R;
    S = QQ[t_1..t_(numrows A)];
    F = map(S, R, apply(numcols(A), i -> S_(flatten entries A_i)));
    B = basis(1, source F) | basis(2, source F) | basis(3, source F);
    lats = unique apply(flatten entries B, i -> degree(sub(i, dom)));
    basisHash = hashTable apply(lats, deg -> {deg, sub(basis(deg, dom), R)});
    trimBasisInDegree({2,1,0,1,1},  dom, {x_2*x_4-x_1*x_5, x_3*x_4-x_1*x_6, x_3*x_5-x_2*x_6}, basisHash)
  Text
    Observe that after trimming we get a smaller monomial basis for this homogeneous component. The full monomial basis is
  Example
    basis({2,1,0,1,1}, dom)
///




doc ///
Key
  componentsOfKernel
  (componentsOfKernel, Number, RingMap)
Headline
  Finds all minimal generators up to a given total degree in the kernel of a ring map 
Usage
  componentsOfKernel(d, F)
Inputs
  d:List
    the total degree at which to stop computing generators
  F:RingMap
  Grading => Matrix
    a matrix whose columns give a homogeneous multigrading on $\ker(F)$
  UseMatroid => Boolean
    use the jacobian of $F$ to detect if a homogeneous component may contain polynomials in the kernel. 
  UseInterpolation => Boolean
    use interpolation instead of symbolically computing polynomials in each homogeneous component.
  CoefficientRing => Ring
    the ground field which is used to sample points from $V(\ker(F))$
  Verbose => Boolean
    display detailed output
Outputs
  :MutableHashTable
    A mutable hashtable whose keys correspond to all homogeneous components of $\ker(F)$ and values correspond to generators in $\ker(F)$ with those components
--Consequences
--  asd
Description
  Text
    Computes all minimal generators of $\ker(F)$ which are of total degree at most {\tt d}. 
  Example
    A = matrix {{1,1,1,0,0,0}, {0,0,0,1,1,1}, {1,0,0,1,0,0}, {0,1,0,0,1,0}, {0,0,1,0,0,1}}
    R = QQ[x_(1,1)..x_(2,3)];
    S = QQ[t_1..t_2, s_1..s_3];
    F = map(S, R, {t_1*s_1, t_1*s_2, t_1*s_3, t_2*s_1, t_2*s_2, t_2*s_3})
    peek componentsOfKernel(2, F)
  Text
    If a grading in which $\ker(F)$ is homogeneous is already known or a specific grading is desired then the option {\tt Grading} can be used to specify this.
    In this case the columns of the matrix {\tt Grading} are automatically used to grade the source of $F$.
Caveat
  If the option UseInterpolation is set to true then @TO2{interpolateComponent, "interpolateComponent"}@ is used to compute polynomials in each homogeneous component
  instead of @TO2{computeComponent, "computeComponent"}@ which is the default. In this case, the resulting polynomials are no longer guaranteed to be in $\ker(F)$ but instead belong to the ideal with high probability.
  Setting this option to true will often significantly speed up computation, especially when computing high degree polynomials in the kernel of dense polynomial maps.
///


doc ///
Key
  computeComponent
  (computeComponent, List, Ring, RingMap, Matrix)
  (computeComponent, List, Ring, RingMap, HashTable)
  (computeComponent, List, Ring, RingMap)
Headline
  Finds all minimal generators of a given multidegree in the kernel of a ring map 
Usage
  computeComponent(deg, dom, F, M)
  computeComponent(deg, dom, F, B)
  computeComponent(deg, dom, F)
Inputs
  deg:List
    the degree of the homogeneous component to compute
  dom:Ring
    a graded ring which is the source of a homogeneous ring map $F$
  F:RingMap
    a map whose kernel is homogeneous in the grading of {\tt dom}
  M:Matrix
    a monomial basis for the homogeneous component of {\tt deg} with degree {\tt deg}
  B:HashTable
    a hashtable which contains all bases of homogeneous components which correspond to lower total degrees than {\tt deg}
  PreviousGens => List
    a list of generators of the kernel which have lower total degree
Outputs
  :Matrix
    A list of minimal generators for $\ker(F)$ which are in the homogeneous component of degree {\tt deg}
--Consequences
--  asd
Description
  Text
    Computes all minimal generators of $\ker(F)$ which are in the homogeneous component of degree {\tt deg}
  Example
    A = matrix {{1,1,1,0,0,0}, {0,0,0,1,1,1}, {1,0,0,1,0,0}, {0,1,0,0,1,0}, {0,0,1,0,0,1}}
    R = QQ[x_(1,1)..x_(2,3)];
    S = QQ[t_1..t_2, s_1..s_3];
    F = map(S, R, {t_1*s_1, t_1*s_2, t_1*s_3, t_2*s_1, t_2*s_2, t_2*s_3})
    dom = newRing(R, Degrees => A);
    computeComponent({1,1,0,1,1}, dom, F)
  Text
      The option {\tt PreviousGens} can be used to specify a set of previously computed generators. 
      In the case that a monomial basis or hash table of monomial bases is not given then @TO2{trimBasisInDegree, "trimBasisInDegree"}@
      will be used to compute a monomial basis and {\tt PreviousGens} will be used to trim this basis. 
///


doc ///
Key
  interpolateComponent
  (interpolateComponent, List, Matrix)
  (interpolateComponent, List, Ring, List, HashTable)
  (interpolateComponent, List, Ring, RingMap)
Headline
  Finds all minimal generators of a given multidegree in the kernel of a ring map by sampling points in the corresponding variety and then interpolating. 
Usage
  interpolateComponent(P, M)
  interpolateComponent(deg, dom, F, B)
  interpolateComponent(deg, dom, F)
Inputs
  deg:List
    the degree of the homogeneous component to compute
  dom:Ring
    a graded ring which is the source of a homogeneous ring map $F$
  F:RingMap
    a map whose kernel is homogeneous in the grading of {\tt dom}
  M:Matrix
    a monomial basis for the homogeneous component of {\tt deg} with degree {\tt deg}
  P:List
    a list of options which correspond to points from $V(\ker(F))$ which are used to interpolate the monomials in {\tt M}
  B:HashTable
    a hashtable which contains all bases of homogeneous components which correspond to lower total degrees than {\tt deg}
  PreviousGens => List
    a list of generators of the kernel which have lower total degree
Outputs
  :Matrix
    A list of minimal generators for $\ker(F)$ which are in the homogeneous component of degree {\tt deg}
Description
  Text
    Computes all minimal generators of $\ker(F)$ which are in the homogeneous component of degree {\tt deg}
  Example
    A = matrix {{1,1,1,0,0,0}, {0,0,0,1,1,1}, {1,0,0,1,0,0}, {0,1,0,0,1,0}, {0,0,1,0,0,1}}
    R = QQ[x_(1,1)..x_(2,3)];
    S = QQ[t_1..t_2, s_1..s_3];
    F = map(S, R, {t_1*s_1, t_1*s_2, t_1*s_3, t_2*s_1, t_2*s_2, t_2*s_3})
    dom = newRing(R, Degrees => A);
    interpolateComponent({1,1,0,1,1}, dom, F)
  Text
    The option {\tt PreviousGens} can be used to specify a set of previously computed generators. 
    In the case that a monomial basis or hash table of monomial bases is not given then @TO2{trimBasisInDegree, "trimBasisInDegree"}@
    will be used to compute a monomial basis and {\tt PreviousGens} will be used to trim this basis. 
Caveat
  The polynomials returned by this method belong to $\ker(F)$ with high probability but may not actually belong to the kernel.
  Further verification is recommended to ensure that the resulting polynomials actually belong to $\ker(F)$.
///


doc ///
Key
  Grading
  [componentsOfKernel, Grading]
Headline
  a matrix which gives a homogeneous multigrading on a polynomial map
--Usage
--Inputs
--Outputs
--Consequences
--  Item
Description
  Text
    The option Grading is a @TO2{Matrix,"matrix"}@ that allows one to specify a specific multigrading in which a polynomial map is homogeneous in.
--
--  CannedExample
--Subnodes
--Caveat
--SeeAlso
///


doc ///
Key
  UseMatroid
  [componentsOfKernel, UseMatroid]
Headline
  use the algebraic matroid of a polynomial map represented by the jacobian to skip computation of irrelevant components of the kernel
--Usage
--Inputs
--Outputs
--Consequences
--  Item
Description
  Text
    The option UseMatroid is a boolean that allows one to specify if the matroid given by the jacobian should be used to automatically skip components. This option is true by default.
    If it is set to false, then every homogeneous component will be checked, even if it is impossible for a polynomial with the necessary support to belong to the kernel.
    For very small examples, it may be slightly faster to set this to false. 
--
--  CannedExample
--Subnodes
--Caveat
--SeeAlso
///


doc ///
Key
  ReturnTargetGrading
  [maxGrading, ReturnTargetGrading]
Headline
  return the grading on the target ring of a polynomial map which induces a grading on the kernel
--Usage
--Inputs
--Outputs
--Consequences
--  Item
Description
  Text
    The option ReturnTargetGrading is a @TO2{Boolean,"boolean"}@ which can be used with @TO2{maxGrading, "maxGrading"}@. This option is false by default. If it is set to true
    then the full grading on the elimination ideal of a polynomial map $F$ will be returned instead of only returning the part of the grading which corresponds to the source of $F$.  
--
--  CannedExample
--Subnodes
--Caveat
--SeeAlso
///


doc ///
Key
  PreviousGens
  [computeComponent, PreviousGens]
  [interpolateComponent, PreviousGens]
Headline
  a list previously computed generators of the kernel of a map
--Usage
--Inputs
--Outputs
--Consequences
--  Item
Description
  Text
    The option PreviousGens is a @TO2{List,"list"}@ of polynomials of total degree at most $d-1$ which can be used with @TO2{computeComponent, "computeComponent"}@
    to trim the monomial basis for that multidegree. 
--
--  CannedExample
--Subnodes
--Caveat
--SeeAlso
///


doc ///
Key
  UseInterpolation
  [componentsOfKernel, UseInterpolation]
Headline
  use interpolation to find polynomials in the kernel of a map
--Usage
--Inputs
--Outputs
--Consequences
--  Item
Description
  Text
    This option is a boolean that allows one to specify if interpolation should be used to find polynomials in each homogeneous component of $\ker(F)$. 
    This option is false by default. If it is set to false then @TO2{computeComponent, "computeComponent"}@ will be used to find polynomials in each homogeneous component which is purely symbolic. 
    If it is set to true then @TO2{interpolateComponent, "interpolateComponent"}@ will be used to to find polynomials in each homogeneous component which are in the kernel with high probability. 
    Setting this option to true will often significantly speed up the computation for large polynomial maps, especially in higher degree components. 
--
--  CannedExample
--Subnodes
--Caveat
--SeeAlso
///


doc ///
Key
  CoefficientRing
  [componentsOfKernel, CoefficientRing]
Headline
  ground field over which to sample points from during interpolation
--Usage
--Inputs
--Outputs
--Consequences
--  Item
Description
  Text
    This option can be used to specify the ground field over which to sample points from the image of a polynomial map. 
    The default value is {\tt ZZ/32003}.
--
--  CannedExample
--Subnodes
--Caveat
--SeeAlso
///


doc ///
Key
  [componentsOfKernel, Verbose]
Headline
  display detailed output during computation
--Usage
--Inputs
--Outputs
--Consequences
--  Item
Description
  Text
    This option is a boolean that determines if detailed output should be displayed during the kernel computation. This option is true by default.
--
--  CannedExample
--Subnodes
--Caveat
--SeeAlso
///
