-*
doc ///
  Key
  Headline
  Usage
  Inputs
  Outputs
  Description
    Text
    Example
  Caveat
  SeeAlso
///
*-

undocumented {
  (oscRing, Graph, List),
  Symbols,
  trig,
  (trig, Ring),
  Stable,
  Semistable,
  Unstable,
  Modulo,
  Radians,
  numOscillators
}

doc ///
Key
  Oscillators
Headline
  generation and analysis of oscillator steady states for small graphs
Description
  Text
    This package supports computations with Kuramoto oscillators,
    including computations for the paper [HSS], Harrington, Schenck,
    Stillman, @arXiv("2312.16069", "Algebraic aspects of homogeneous
    Kuramoto oscillators")@.  For a list of functions, and links to their documentation
    nodes, see the end of this page.
  Text
    @SUBSECTION "Computations from the paper [HSS]"@
  Text
    @UL {
        TO "Generation of all SCT (simple, connected, 2-connected) graphs on small numbers of vertices",
        TO "Checking the codimension and irreducible decomposition of the IG ideal",
        TO "SCT graphs with exotic solutions",
        TO "Example 4.1: unique graph on 8 vertices with exotic solutions and no induced cycle of length at least 5",
        TO "Example 4.2: a K5 and pentagon glued along an edge",
        TO "Example 4.3: examples of gluing two cycles along an edge",
        TO "Example 4.4: The square within a square"
        }@
  Text
    We show a possible workflow using this package.  We use @TO "NautyGraphs::NautyGraphs"@
    to generate graphs of small size.  We use @TO "Visualize::Visualize"@ to look at these graphs.
  Example
    needsPackage "Oscillators";
    needsPackage "NautyGraphs";
    needsPackage "Visualize";
  Text
    Let's do an example: the 5-cycle.  First, we generate all SCT graphs on 5 vertices (i.e. connected, 2-connected simple graphs),
    and grab the 5-cycle.
  Example
    Gstrs = generateGraphs(5, OnlyConnected => true, MinDegree => 2);
    Gs = Gstrs/stringToGraph
    Gcycle5 = Gs_3
  Text
    We can also create the graph directly.
  Example
    Gcycle5 == graph{{0,1},{1,2},{2,3},{3,4},{4,0}}
  Text
    To visualize this graph, use the following lines.  You have to click on End session
    in the browser to get back to Macaulay2.
  Pre
    openPort "8083"
    visualize Gcycle5 -- important: click on End session in browser window before continuing
    closePort()
  Text
    We construct all of the (steady-state) solutions to the homogeneous Kuramoto system for the 5-cycle.
    This includes all solutions, not just stable solutions.
    In this example, we first construct the real solutions, and check their (linear) stability.
    Then we find all complex solutions.  In this particular example, all 30 solutions are real.
    We set the precision displayed to 3 digits.
  Example
    printingPrecision = 3
    G = Gcycle5
    R = oscRing(G, CoefficientRing => CC, Reduced => true)
    I = oscSystem(G, R);
    netList I_*
    Jac = oscJacobian(G,R)
    realsols = findRealSolutions I;
    netList realsols
    assert(# realsols == 30)
    stablesols = select(realsols, p -> Stable === identifyStability(Jac,p))
    tally realsols/(p -> identifyStability(Jac, p))
    getAngles(4, stablesols, Radians=>false)
    sols = solveSystem I_*;
    sols = sols/coordinates;
    sols = matrix sols -- every row is a solution
    sols = clean(1e-6, sols) -- set to 0 numbers very close to 0
    assert(numrows sols == 30) -- all the solutions in this example are real
    tally (entries sols)/(p -> identifyStability(Jac, p))
  Text
    For the further analysis of these ideals, see @TO "oscQuadrics"@.
///

///
Key
  Oscillators
Headline
  generation and analysis of oscillator steady states for small graphs
Description
  Text
    We show a possible workflow using this package.  We use @TO "NautyGraphs::NautyGraphs"@
    to generate graphs of small size.  We use @TO "Visualize::Visualize"@ to look at these graphs.
  Example
    needsPackage "Oscillators"
    needsPackage "NautyGraphs"
    needsPackage "Visualize"
    vert4edges4 = generateGraphs(4, 4)
    Gs = vert4edges4/stringToGraph
  Text
    A slightly larger example.
  Example
    vert7edges10 = generateGraphs(7, 10);
    Gs = vert7edges10/stringToGraph
  Text
    Now, for each graph we consider the oscillator dynamical system.
    
    Let's do an example, using a graph with 5 vertices and 7 edges
  Example
    Gstrs = generateGraphs(5,7);
    Gs = Gstrs/stringToGraph
    G = Gs_1
  Pre
    openPort "8083"
    visualize G -- important: click on End session in browser window before continuing
    closePort()
  Example
    R = oscRing(G, CoefficientRing => CC, Reduced => true)
    I = oscSystem(G,R);
    Jac = oscJacobian(G,R)
    realsols = findRealSolutions I
    assert(# realsols == 24)
    stablesols = select(realsols, p -> Stable === identifyStability(Jac,p))
    tally realsols/(p -> identifyStability(Jac, p))
    getAngles(4, stablesols, Radians=>false)
    sols = solveSystem I_*;
    sols = sols/coordinates
    assert(#sols == 36)
    tally sols/(p -> identifyStability(Jac, p))
    netList sols
///

doc ///
  Key
    oscRing
    (oscRing, Graph)
    (oscRing, ZZ)
    [oscRing, Reduced]
    [oscRing, CoefficientRing]
    [oscRing, Symbols]
  Headline
    create a polynomial ring for a given graph or number of oscillators
  Usage
    S = oscRing G
    S = oscRing n
  Inputs
    n:ZZ
      The number of oscillators
    G:Graph
      The number of oscillators is the number of vertices in the Graph
    CoefficientRing => Ring
      the coefficient ring to use, for numerical work, @TO "CC"@ is a good choice
    Symbols => Sequence
      a sequence of two symbols.  The first refers to the cosine of the given angles
        and the second refers to the sine of the given angles
    Reduced => Boolean
      if true, then the angles are reduced to the first angle being 0. That is, the number of oscillators will be one less than the number of vertices of $G$
  Outputs
    :Ring
  Description
    Text
      This function returns a polynomial ring in $2n$ variables
      representing the cosine and the sine of $n$ angles. Inputting a number assumes that the vertices are labeled $0, \ldots, n$.
    Example
      oscRing(3, CoefficientRing => CC)
      oscRing(3, CoefficientRing => CC, Symbols=>{c, s})
    Text
      If one gives a graph with Reduced => true, then the number of angles considered
      is one less than the number of vertices of the graph.
    Example
      G = graph({0,1,2,3}, {{0,1},{1,2},{2,3},{0,3}})
      oscRing G
      oscRing(G, CoefficientRing => CC, Reduced => true)
  SeeAlso
    oscSystem
    oscJacobian
    displayGraph
///

doc ///
  Key
    oscSystem
    (oscSystem, Graph, Ring)
    (oscSystem, Graph)
    [oscSystem, Reduced]
  Headline
    the ideal of the reduced equilibrium points of a dynamical system of oscillators
  Usage
    oscSystem(G,R)
    oscSystem(G)
  Inputs
    G:Graph
      an undirected, connected graph $G$
    R:Ring
      created with @TO oscRing@
    Reduced => Boolean
      if true, then the angles are reduced to the first angle being 0. That is, the number of oscillators will be one less than the number of vertices of $G$
  Outputs
    :Ideal
      in the ring R
  Description
    Text
      $R$ should be a ring created with @TO "oscRing"@.  The dynamical system
      involved is the oscillator system associated to $G$: one angle per vertex.
      If $a_{ij} = 1$ if $(i,j)$ is an edge of the undirected graph $G$, 
      and is zero otherwise, then the system is $d\theta_i/dt = \sum_j a_{ij} \sin(\theta_j - \theta_i)$
      where we consider only reduced equilibrium solutions $\theta_0 = 0$.
      
      This function returns the ideal of equilibrium points, where angles $(0, \theta_1, ..., \theta_{n-1})$
      are represented via cosines and sines of the angles.
    Example
      G = graph({0,1,2,3}, {{0,1},{1,2},{2,3},{0,3}})
      oscRing(G, CoefficientRing => CC)
      R = oo
      I = oscSystem(G,R)
      netList I_*
    Text
      We can find approximations to the 26 complex solutions to this system.
      If the system has positive dimension (not the case here), the idea is that this set of points should
      contain at least one on each component.
    Example
      solveSystem I_*
      #oo
    Text
      We can find approximations to the 6 real solutions to this system.  
    Example
      findRealSolutions I
      #oo
    Text
      The angles of these solutions (in degrees, not radians, and the 3 refers to the
      numbner of oscillators).
    Example
      netList getAngles(3, findRealSolutions I, Radians=>false)
  SeeAlso
      oscRing
      oscJacobian
      findRealSolutions
      getAngles
      solveSystem
///

doc ///
  Key
    oscJacobian
    (oscJacobian,Graph,Ring)
    (oscJacobian, Graph)
    (oscJacobian, Ideal)
    [oscJacobian, Reduced]
  Headline
    create the Jacobian for the oscillator system associated to a graph
  Usage
    oscJacobian(G,R)
    oscJacobian(I)
    oscJacobian(G)
  Inputs
    G:Graph
      an undirected, connected graph $G$ 
    S:Ring
      created with @TO oscRing@
    I:Ideal
      an ideal, intended to be ideal created by oscSystem
    Reduced => Boolean
      if true, then the angles are reduced to the first angle being 0. That is, the number of oscillators will be one less than the number of vertices of $G$
  Outputs
    :Matrix
      the $n \times n$ Jacobian matrix of the system, as a matrix of polynomials
      involving the cosines and sines of angles $\theta_1, \ldots, \theta_{n-1}$, where we
      set $\theta_0 = 0$ if $\texttt{Reduced => true}$
  Description
    Text
      The matrix is a symmetric $n \times n$  matrix (with determinant zero).
    Example
      G = graph({0,1,2,3}, {{0,1},{1,2},{2,3},{0,3}})
      oscRing(G, CoefficientRing => CC)
      S = oo
      I = oscSystem(G,S)
      Jac = oscJacobian(G,S)
      assert(det Jac == 0)
      assert(Jac - transpose Jac == 0)
    Text
      We can find the eigenvalues of the Jacobian at approximate points, and see if they are
      stable (all eigenvalues negative, except for the one required 0), unstable (a positive 
      eigenvalue), or semistable (no positive eigenvalues, up to a certain tolerance).
    Example
      realsols = findRealSolutions I
      jacs = for pt in realsols list sub(Jac, matrix{pt})
      jacs/eigenvalues
      jacs/eigenvalues/identifyStability
  SeeAlso
    oscRing
    oscSystem
    findRealSolutions
    identifyStability
    eigenvalues
///

doc ///
  Key
    findRealSolutions
    (findRealSolutions, Ideal)
    (findRealSolutions, Graph)
  Headline
    find real solutions, at least one per component for well-conditioned systems
  Usage
    findRealSolutions I
    findRealSolutions G
  Inputs
    I:Ideal
      an ideal in a polynomial ring over QQ or RR or CC.
    G:Graph
      an undirected, connected graph $G$
  Outputs
    :List
      of all the real solutions that were found
  Description
    Text
      In this package the main use is to zero in on equilibrium points for oscillator 
      systems associated to graphs.
      
      We use this in the following example.
    Example
      G = graph({0,1,2,3,4}, {{0,1},{1,2},{2,3},{0,3},{0,4}})
      S = oscRing(G, CoefficientRing => QQ, Reduced => true)
      I = oscSystem(G,S)
      dim I
      Jac = oscJacobian(G,S)
      assert(det Jac == 0)
      assert(Jac - transpose Jac == 0)
      realsols = findRealSolutions I      
      netList getAngles(4, realsols, Radians=>false)
      C = decompose I
      for i in C list Jac % i
      M = Jac % C_1
      M1 = matrix{{-1,0,0,0,1},{0,0,0,0,0},{0,0,0,0,0},{0,0,0,0,0},{1,0,0,0,-1}}
      use S
      eigenvalues lift(sub(M - M1, x_3=>1), QQ)
      eigenvalues lift(sub(M, x_3=>1), QQ)
      eigenvalues lift(sub(M, x_3=>1/100000), QQ)
      eigenvalues lift(sub(M, x_3=>100), QQ)
  Caveat
    If the system is positive dimensional, then of course it won't find all roots. There might be
    real roots on a component, but none are found. If a component (even a point) is singular, then
    it might have problems, depending on the situation. In the latter case, there
    is a warning message emitted (about non-regular solutions).
  SeeAlso
    oscRing
    oscSystem
    oscJacobian
    identifyStability
///

doc ///
  Key
    oscQuadrics
    (oscQuadrics, Graph, Ring)
    (oscQuadrics, Graph)
    [oscQuadrics, Reduced]
  Headline
    find the homogeneous quadrics in the homogeneous Kuramoto ideal
  Usage
    oscQuadrics(G,R)
    oscQuadrics(G)
  Inputs
    G:Graph
      an undirected, connected graph $G$ on vertices $0, \ldots, n-1$, where $n$
      is the number of vertices of $G$
    R:Ring
      created with @TO oscRing@
    Reduced => Boolean
      if true, then the angles are reduced to the first angle being 0. That is, the number of oscillators will be one less than the number of vertices of $G$
  Outputs
    :Ideal
      in the ring R
  Description
    Text
      The ideal of quadrics of the oscillator system associated to the graph $G$.
      This is the ideal generated by the quadrics of the system, where the system is
      $d\theta_i/dt = \sum_j a_{ij} \sin(\theta_j - \theta_i)$
      where we consider only reduced equilibrium solutions $\theta_0 = 0$.
    Example
      G = graph({0,1,2,3}, {{0,1},{1,2},{2,3},{0,3}})
      oscRing(G, CoefficientRing => CC)
      S = oo
      I = oscQuadrics(G,S)
      netList I_*
///

doc ///
Key
  standardSols
  (standardSols, Graph, Ring)
  (standardSols, Graph)
  [standardSols, Reduced]
Headline
  find the "standard solutions" for the oscillator system associated to a graph
Usage
  standardSols(G,R)
  standardSols(G)
Inputs
  G:Graph
    an undirected, connected graph $G$
  R:Ring
    created with @TO oscRing@
  Reduced => Boolean
    if true, then the angles are reduced to the first angle being 0. That is, the number of oscillators will be one less than the number of vertices of $G$
Outputs
  :Ideal
    a list of the "standard solutions" that you get where all the angles are the same. This is the same as a certain Segre variety.
Description
  Text
    The oscillator ideal associated to a graph constructed by @TO oscQuadrics@ always contains the "standard solutions" as a minimal prime. These standard solutions are the Segre variety $\mathbb{P}^1\times \mathbb{P}^{n-1}$, where $n$ is the number of vertices of the graph. These are the solutions where all the angles are the same.
  Example
    G = graph({0,1,2,3}, {{0,1},{1,2},{2,3},{0,3}});
    R = oscRing(G);
    I = oscQuadrics(G, R);
    any(decompose I, P -> P == standardSols(G, R))
SeeAlso
  oscRing
  oscSystem
  oscJacobian
///

doc ///
Key 
  vertexSpanningPolynomial
  (vertexSpanningPolynomial, Graph)
  (vertexSpanningPolynomial, Graph, Ring)
Headline
  computes the vertex spanning polynomial
Usage
  vertexSpanningPolynomial(G)
  vertexSpanningPolynomial(G, R)
Inputs
  G:Graph
    an undirected, connected graph $G$
  R:Ring
    created with @TO oscRing@
Outputs
  :PolynomialRing
    the vertex spanning polynomial of the graph $G$ inside $R$.
Description
  Text
    Let $S$ be the set of all spanning trees of the graph $G$. For each spanning tree $T$, let $d_i$ be the degree of $v_i$ in $T$. The vertex spanning polynomial of a graph $G$ is defined as $\sum_{T\in S} \prod_{v_i \in T} x_i^{d_i-1}$. The factorization of this polynomial is related to the number of components of the oscillator ideal of the graph computed via @TO oscQuadrics@.
  Example
    G = graph({0,1,2,3}, {{0,1},{1,2},{2,3},{0,3}});
    vertexSpanningPolynomial G
SeeAlso
  oscQuadrics
///

doc ///
Key
  getAngles
  (getAngles, ZZ, List)
  [getAngles, Radians]
Headline
  Compute angles from a list of solutions
Usage
  getAngles(n, S)
Inputs
  n: ZZ
    An integer representing the number of angles.
  sols: List
    A list of solutions, where each solution is a list of cosine and sine values.
  Radians => Boolean
    An optional boolean flag (default is true). If true, the angles are returned in radians; otherwise, they are returned in degrees.
Outputs
  : List
    A list of angles computed from the given solutions.
Description
  Text
    The function getAngles computes the angles from a list of solutions. Each solution is assumed to be a list of cosine and sine values for the angles. The function returns the angles in radians or degrees based on the Radians option.
  Example
    getAngles(2, {{1, 0, 0, 1}, {0, 1, 1, 0}}, Radians => false)
SeeAlso
  getLinearlyStableSolutions
  findRealSolutions
///

doc ///
Key
  identifyStability
  (identifyStability, Matrix, List)
  (identifyStability, BasicList)
  [identifyStability, Tolerance]
Headline
  Identify the stability of a list of eigenvalues, or of potential solutions to the oscillator system
Usage
  identifyStability(Jac, sols)
  identifyStability(eigenvals)
Inputs
  Jac: Matrix
    A matrix representing the Jacobian of the oscillator system.
  sol: List
    A potential solution to the oscillator system.
  eigenvals: BasicList
    An eigenvalue
  Tolerance => RR
    An optional real number representing the tolerance for the stability check (default is 1e-10).
Outputs
  : BasicList
    A list of stability values for the given solutions or eigenvalues.
Description
  Text
    The function identifyStability computes the stability of a list of solutions or eigenvalues. For solutions, the function computes the eigenvalues of the Jacobian at each solution and determines the stability based on the sign of the real part of the eigenvalues. For eigenvalues, the function determines the stability based on the sign of the real part of the eigenvalues.
  Example
    G = graph({0,1,2,3}, {{0,1},{1,2},{2,3},{0,3}});
    R = oscRing(G, Reduced => true);
    I = oscSystem(G, R);
    Jac = oscJacobian(G, R);
    realsols = findRealSolutions I;
    jacs = for pt in realsols list sub(Jac, matrix{pt});
    eigenvals = jacs/eigenvalues;
    eigenvals / identifyStability
    realsols/(pt -> prepend(identifyStability(Jac, pt), pt))
SeeAlso
  oscJacobian
  findRealSolutions
///

doc ///
Key
  getLinearlyStableSolutions
  (getLinearlyStableSolutions, Graph)
Headline
  Compute linearly stable solutions for the Kuramoto oscillator system associated to a graph
Usage
  getLinearlyStableSolutions(G)
Inputs
  G: Graph
    An undirected, connected graph
Outputs
  : List
    A list of linearly stable solutions for the Kuramoto oscillator system associated to the graph
Description
  Text
    The function getLinearlyStableSolutions computes the linearly stable solutions for the Kuramoto oscillator system associated to a given graph. The Kuramoto oscillator system is a system of coupled phase oscillators, where the dynamics of each oscillator is given by the Kuramoto model. The linear stability of a solution is determined by the eigenvalues of the Jacobian matrix of the system evaluated at the solution.
  Example
    G = graph({0,1,2,3}, {{0,1},{1,2},{2,3},{0,3}});
    getLinearlyStableSolutions(G)
SeeAlso
  findRealSolutions
  identifyStability
///

doc ///
Key
  showExoticSolutions
  (showExoticSolutions, Graph)
  getExoticSolutions
  (getExoticSolutions, Graph)
Headline
  Display exotic solutions: linearly stable solutions which are not all-in-phase solution
Usage
  showExoticSolutions G
  getExoticSolutions G
Inputs
  G: Graph
    An undirected, connected graph
Outputs  
  : List
    A list of all of the linearly stable solutions for the Kuramoto oscillator system associated to the graph
Consequences
  Item
    The list of exotic solutions is displayed, together with the angles of the oscillators (where the first angle is always zero, and not displayed)
Description
  Text
    This function first calls @TO getLinearlyStableSolutions@, and then displays any exotic solutions that exist.
    A stable solution is exotic if it is not the all-in-phase solution (all the angles are the same),
    and returns all of the stable solutions found.

    Note: The methods {\tt getExoticSolutions} and {\tt showExoticSolutions} are the same, for historical reasons.
    
    The warning that there are non-regular solutions generally means that it has come across a positive dimensional
    solution set while looking for solutions.  No such solutions can be linearly stable, so the warning is generally not relevant.
  Example
    G = graph {{0,1},{1,2},{2,3},{3,4},{4,0}}
    showExoticSolutions G
  Text
    Note that if there are exotic solutions, all linearly stable solutions are displayed, and if there are no exotic solutions,
    nothing is displayed (other than possibly warnings and timings).
  Example
    G = graph {{0,1},{1,2},{2,3},{3,4},{4,2},{4,0}}
    showExoticSolutions G
SeeAlso
  getLinearlyStableSolutions
  findRealSolutions
  identifyStability
///

doc ///
Key
  allUniquePrincipalMinors
  (allUniquePrincipalMinors, Matrix)
  [allUniquePrincipalMinors, Modulo]
Headline
  Compute all unique principal minors of a given matrix
Usage
  allUniquePrincipalMinors(M)
Inputs
  M: Matrix
    A square matrix
  Modulo => Ideal
    An optional ideal to compute the principal minors modulo the given ideal
Outputs 
  : List
    A list of all unique principal minors of the given matrix
Description
  Text
    The function allUniquePrincipalMinors computes all unique principal minors of a given square matrix. A principal minor of a matrix is the determinant of a submatrix obtained by deleting the same set of rows and columns from the matrix. The function returns a list of all unique principal minors of the given matrix.
  Example
    G = graph({0,1,2,3},{{0,1},{1,2},{2,3},{0,3}})
    S = oscRing(G, CoefficientRing => QQ, Reduced => true)
    I = oscSystem(G,S);
    C = decompose I;
    J = oscJacobian(G,S)
    netList for i in C list allUniquePrincipalMinors(-J, Modulo=>i)
    -- by looking at each one, all points but one are unstable.
SeeAlso
  oscJacobian
  identifyStability
///

doc ///
Key
  isStableSolution
  (isStableSolution, Matrix, List)
Headline
  Check if a given solution is stable for the Kuramoto oscillator system
Usage
  isStableSolution(Jac, sol)
Inputs
  Jac: Matrix
    A matrix representing the Jacobian of the Kuramoto oscillator system
  sol: List
    A potential solution to the Kuramoto oscillator system
Outputs
  : Boolean
    True if the solution is stable, false otherwise
Description
  Text
    The function isStableSolution checks if a given solution is stable for the Kuramoto oscillator system. The stability of a solution is determined by the eigenvalues of the Jacobian matrix of the system evaluated at the solution. If all eigenvalues have negative real parts, the solution is considered stable.
  Example
    G = graph({0,1,2,3}, {{0,1},{1,2},{2,3},{0,3}});
    R = oscRing(G, Reduced => true);
    I = oscSystem(G, R);
    Jac = oscJacobian(G, R);
    realsols = findRealSolutions I;
    select(realsols, S -> isStableSolution(Jac, S))
SeeAlso
  oscJacobian
  findRealSolutions
  identifyStability
///


doc ///
  Key
    "Generation of all SCT (simple, connected, 2-connected) graphs on small numbers of vertices"
  Headline
    generating all SCT graphs on n vertices
  Description
    Text
      Using the NautyGraphs package, we generate the list of
      isomorphism classes of the SCT (simple, connected, 2-connected)
      graphs with a fixed number of vertices.
    Example
      needsPackage "Oscillators"
      needsPackage "NautyGraphs"
      Gstrs = generateGraphs(5, OnlyConnected => true, MinDegree => 2);
      #Gstrs == 11
      Gstrs = generateGraphs(6, OnlyConnected => true, MinDegree => 2);
      #Gstrs == 61
      Gstrs = generateGraphs(7, OnlyConnected => true, MinDegree => 2);
      #Gstrs == 507
      Gstrs = generateGraphs(8, OnlyConnected => true, MinDegree => 2);
      #Gstrs == 7442
      Gstrs = generateGraphs(9, OnlyConnected => true, MinDegree => 2);
      #Gstrs == 197772
    Text
      Here is a simple table with all of these numbers.
    Example
      netList for n from 5 to 9 list {n, #generateGraphs(n, OnlyConnected => true, MinDegree => 2)}
  SeeAlso
///

-- XXX

doc ///
  Key
    "Checking the codimension and irreducible decomposition of the IG ideal"
  Headline
    generating all SCT graphs on n vertices
  Description
    Text
      We first construct the ideal $I_G$ for a specific graph $G$ on 5 vertices.
      We use the 5-cycle as the specific example.
    Example
      needsPackage "Oscillators"
      needsPackage "NautyGraphs"
      Gstrs = generateGraphs(5, OnlyConnected => true, MinDegree => 2);
      Gs = Gstrs/stringToGraph
      G = Gs_3
      R = oscRing(5, Reduced => false);
      IG = oscQuadrics(G, R)
      netList IG_*
      codim IG
    Text
      Each $I_G$ on $n$ vertices has $n-1$ minimal generators.
      This particular $I_G$ has the same codimension $n-1=4$, so is a complete intersection.
      This ideal decomposes as an intersection of 2 prime ideals.
    Example
      comps = decompose IG;
      netList comps_0_*, netList comps_1_*
      comps/codim
      comps/degree
      comps/isPrime
    Text
      Let's see how many graphs are not complete intersections, i.e. have codimension
      $\le n-2$.
    Example
      # select(Gs, G -> (
          IG = oscQuadrics(G, R);
          codim IG <= #vertices G - 2
          ))
    Example
      for G in Gs list (
          IG = oscQuadrics(G, R);
          elapsedTime comps := decompose IG;
          {comps/codim, comps/degree}
          );
      netList oo
    Example
      n = 6
      Gstrs = generateGraphs(n, OnlyConnected => true, MinDegree => 2);
      Gs = Gstrs/stringToGraph;
      R = oscRing(n, Reduced => false);
      # select(Gs, G -> (
          IG = oscQuadrics(G, R);
          codim IG <= #vertices G - 2
          ))
    Example
      allcomps = for G in Gs list (
          IG = oscQuadrics(G, R);
          elapsedTime comps := decompose IG;
          {comps/codim, comps/degree}
          );
      netList ({{"codimensions", "degrees"}} | allcomps)
    Pre
      n = 7
      Gstrs = generateGraphs(n, OnlyConnected => true, MinDegree => 2);
      Gs = Gstrs/stringToGraph;
      R = oscRing(n, Reduced => false);
      # select(Gs, G -> (
          IG = oscQuadrics(G, R);
          elapsedTime codim IG <= #vertices G - 2
          ))
    Text
      The next one takes some time.
    Pre
      n = 8
      Gstrs = generateGraphs(n, OnlyConnected => true, MinDegree => 2);
      Gs = Gstrs/stringToGraph;
      R = oscRing(n, Reduced => false);
      # select(Gs, G -> (
          IG = oscQuadrics(G, R);
          codim IG <= #vertices G - 2
          ))
  
  SeeAlso
///

doc ///
  Key
    "SCT graphs with exotic solutions"
  Headline
    finding graphs of small size with exotic solutions
  Description
    Text
      We first construct the ideal $I_G$ for a specific graph $G$ on 5 vertices.
      We use the 5-cycle as the specific example.
    Example
      needsPackage "Oscillators"
      needsPackage "NautyGraphs"
      Gstrs = generateGraphs(5, OnlyConnected => true, MinDegree => 2);
      Gs = Gstrs/stringToGraph
      printingPrecision = 3
      for G in Gs list showExoticSolutions G;
///

doc ///
  Key
    "Example 4.1: unique graph on 8 vertices with exotic solutions and no induced cycle of length at least 5"
  Headline
    example 4.1 in arXiv 2312.16069
  Description
    Text
      This example is a square inside a square, having 8 vertices,
      and exotic solutions. This is the only SCT graph on 8 vertices
      with exotic solutions, not containing an induced $k$-cycle, for $k \ge 5$.
    Example
      needsPackage "Oscillators";
      printingPrecision = 3;
      G = graph{
          {0,4},{1,4},{1,5},
          {3,5},{2,6},{3,6},
          {0,7},{2,7},{4,5},
          {5,6},{6,7},{4,7}}
      stablesols = {{0, 0, -1, .707, -.707, -.707, .707, -1, 1, 0, -.707, -.707, .707, .707},
          {1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0},
          {0, 0, -1, .707, -.707, -.707, .707, 1, -1, 0, .707, .707, -.707, -.707}}
    Text
      Computing the (linearly) stable solutions for K5C5 takes a minute or two:
    Pre
      elapsedTime stablesols = showExoticSolutions G;
  SeeAlso
    "Harrington-Schenck-Stillman"
///

doc ///
  Key
    "Example 4.2: a K5 and pentagon glued along an edge"
  Headline
    example 4.2 in arXiv 2312.16069
  Description
    Text
      The first example is a pentagon.
      The second example is K5 with a 5-cycle glued along an edge.
    Example
      needsPackage "Oscillators";
      printingPrecision = 3;
      Pent = graph{{0,1},{1,2},{2,3},{3,4},{4,0}}
      K5C5 = graph{{0,1},{0,2},{0,3},{0,4},{1,2},{1,3},{1,4},
          {2,3},{2,4},{3,4},{0,5},{5,6},{6,7},{1,7}}
      stablesolsK5C5 = {{1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0},
          {.92, .98, .98, .98, .101, -.98, -.298, -.393, -.201, -.201, -.201, .995, .201, -.954},
          {.92, .98, .98, .98, .101, -.98, -.298, .393, .201, .201, .201, -.995, -.201, .954}}
      elapsedTime stablesolsPent = showExoticSolutions Pent
    Text
      Computing the (linearly) stable solutions for K5C5 takes a minute or two:
    Pre
          elapsedTime stablesolsK5C5 = showExoticSolutions K5C5
  SeeAlso
    "Harrington-Schenck-Stillman"
///

doc ///
  Key
    "Example 4.3: examples of gluing two cycles along an edge"
  Headline
    example 4.3 in arXiv 2312.16069
  Description
    Text
      The following example has two pentagons glued on an edge, and this example
      has 4 exotic solutions, and it is the unique graph on 8 vertices with
      more than 2 exotic solutions.
    Example
      needsPackage "Oscillators";
      printingPrecision = 3;
      PentPent = graph{{0,1},{1,2},{2,3},{3,4},{4,0},{0,5},{5,6},{6,7},{7,1}}
      stablesolsPentPent = {
          {.648, .796, .908, .977, .215, -.908, -.605, .762, .605, .42, .215, -.977, -.42, .796},
          {1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0},
          {.648, .796, .908, .977, .215, -.908, -.605, -.762, -.605, -.42, -.215, .977, .42, -.796},
          {.648, -.605, -.908, .215, .977, .908, .796, .762, .796, -.42, -.977, .215, .42, .605},
          {.648, -.605, -.908, .215, .977, .908, .796, -.762, -.796, .42, .977, -.215, -.42, -.605}}
      stablesolsHexPent
    Text
      The computation of these (linearly) stable solutions takes perhaps a minute:
    Pre
      elapsedTime stablesolsPentPent = showExoticSolutions PentPent
    Text
      The following example has a hexagon and pentago glued identified on a common edge,
      and this example has 9 vertices and 6 exotic solutions.
    Example
      HexPent = graph{{0,1},{1,2},{2,3},{3,4},{4,0},{0,5},{5,6},{6,7},{7,8},{8,1}}
    Text
      The computation of exotic solutions for this example takes several minutes:
    Pre
      elapsedTime stablesolsHexPent = showExoticSolutions HexPent
  SeeAlso
    "Harrington-Schenck-Stillman"
///


doc ///
  Key
    "Example 4.4: The square within a square"
  Headline
    example 4.4 in arXiv 2312.16069
  Description
    Text
      The following is the one graph on 8 vertices with an exotic solution
      where the Jacobian has some off-diagonal extries negative.
    Example
      needsPackage "Oscillators";
      printingPrecision = 3;
      G = graph {{0,1},{1,2},{2,3},{3,4},{4,5},{5,6},{6,0},{0,5},{0,2},{5,7},{2,7}}
      stablesols = {{1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0},
          {.623, -.223, -.901, -.901, -.223, .623, -1, -.782, -.975, -.434, .434, .975, .782, 0},
          {.623, -.223, -.901, -.901, -.223, .623, -1, .782, .975, .434, -.434, -.975, -.782, 0}}
    Pre
      elapsedTime stablesols = showExoticSolutions G
    Text
      The Jacobian at each exotic solution has negative off diagonl entries.
    Example
      Jac = oscJacobian(G, Reduced => true)
      sub(Jac, matrix{stablesols#1})
      sub(Jac, matrix{stablesols#2}) -- this is identical to the previous jacobian
      sub(Jac, matrix{stablesols#0}) -- negative of Laplacian of the graph
  SeeAlso
    "Harrington-Schenck-Stillman"
///


doc ///
  Key
    "Harrington-Schenck-Stillman"
  Headline
    Arxiv 2312.16069 reference
  Description
    Text
      [HSS] @arXiv("2312.16069", "Algebraic examples of homogeneous Kuramoto oscillators")@
            by Heather Harrington, Hal Schenck, Mike Stillman, 2023.
///            
