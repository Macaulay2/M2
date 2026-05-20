doc ///
Node
  Key
    "Tutorial: Numerical algebraic geometry"
  Headline
    Numerical algebraic geometry
  Description
    Text
      @BOLD "Quick start: Solving a system of equations"@

      Suppose that you would like to solve a system of polynomial equations (polynomial system).
      Using Macaulay2, one can simply run the following:

    Example
      needsPackage "NumericalAlgebraicGeometry"
      R = CC[x,y];
      f = {x^2+y^2-2, (x-2)^2-y^2-6}; -- your system of interest
      solveSystem f

    Text
      The system $f$ describes the intersection of a circle and a hyperbola,
      and therefore has four intersection points over the complex numbers.

      Solving polynomial systems is a central problem in algebraic geometry,
      with many applications in areas such as engineering and scientific computing.
    
      {\it Numerical algebraic geometry} uses techniques from numerical analysis
      to find (approximate) solutions of systems of polynomial equations.

      Traditionally, polynomial systems are studied using symbolic methods
      such as Gröbner bases, resultants, and elimination techniques.
      While these methods are powerful, they often become prohibitively slow
      for large or complicated systems.

      Numerical algebraic geometry provides an alternative by replacing exact
      symbolic computations with high-precision numerical approximations.

    Text
      @BOLD "A. Homotopy continuation"@

    Text
      The central tool in numerical algebraic geometry is {\it homotopy continuation}.
      This method constructs a homotopy between two polynomial systems
      and tracks solutions from one system to the other.

      Typically, one system is chosen so that its solutions are already known
      or easy to compute; this is called the {\it start system}.
      The other system is the polynomial system of interest, called the
      {\it target system}.

      One common construction is the {\it straight-line homotopy}.
      Given a target system $f$ and a start system $g$, the homotopy is
      defined by
      $H(x,t) = (1-t)g(x) + tf(x)$.
      Starting from a known solution $x^*$ of $g$ at $t=0$, one tracks the
      solution path as $t$ moves toward $1$, approximating a solution of $f$.

      The package @TO "NumericalAlgebraicGeometry"@ provides fundamental tools
      in numerical algebraic geometry.

    Example
      needsPackage "NumericalAlgebraicGeometry"
      R = CC[x,y]
      g = {x^2-1,y^2-1}
      f = {x^2+y^2-1, (x-y)^2-1}
      solsS = {(1,-1),(1,1),(-1,1),(-1,-1)} -- solutions to g
      track(g,f,solsS)

    Text
      In this example, the start system is a {\it Bézout start system},
      giving a total number of paths equal to the product of the degrees
      of the defining polynomials. This number is an upper bound
      for the number of isolated solutions of the target system.

      The same process can also simply be carried out using:

    Example
      R = CC[x,y];
      f = {x^2+y^2-1, (x-y)^2-1};
      solveSystem f

    Text
      Note that homotopy continuation techniques are typically applied to
      zero-dimensional systems, that is, systems with finitely many solutions.

      This often occurs when the system has the same number of equations
      as variables and the equations are sufficiently generic,
      yielding a complete intersection.

      
    Text
      @BOLD "B. Witness sets and numerical irreducible decomposition"@

    Text
      The idea of homotopy continuation for finding solutions to
      zero-dimensional systems extends to the study of positive-dimensional
      varieties. The key observation is that intersecting a positive-dimensional
      variety with sufficiently many generic linear slices produces a
      zero-dimensional variety.

      The data consisting of a polynomial system, a collection of generic
      linear slices, and the resulting finite set of points is called a
      {\it witness set}. Witness sets provide a numerical representation of
      positive-dimensional algebraic varieties and can be used to compute:

      @UL {
        LI "dimension",
        LI "degree",
        LI "sampling from a variety",
        LI "irreducible decomposition of a variety"
      }@

    Example
      R = CC[x,y,z];
      I = ideal {
          (x^2+y^2+z^2-1)*(y-x^2),
          (x^2+y^2+z^2-1)*(z-x^3)
          };
      W = components numericalIrreducibleDecomposition I
      p = sample first W -- sampling a point from the first component
      evaluate(gens I, p)

    Text
      @BOLD "C. Monodromy methods"@      

    Text
      When homotopy continuation is used to compute all solutions
      of a polynomial system, one typically chooses a start system
      whose number of solutions gives an upper bound
      for the number of isolated solutions of the target system.
      In practice, however, finding a good upper bound may be difficult.

      {\it Monodromy methods} provide a heuristic alternative.
      For a polynomial system depending on parameters, as the parameters vary
      continuously, the solutions trace paths. Following these paths along
      loops in parameter space may transform one solution into another;
      this is the {\it monodromy action}.

      The resulting monodromy action on the solution set is transitive when
      the solution variety is irreducible. Consequently, starting from a subset
      of known solutions, monodromy loops can be used to find all remaining
      solutions.

      The package @TO "MonodromySolver"@ implements these techniques
      for solving polynomial systems numerically.

    Example
      needsPackage "MonodromySolver"
      declareVariable \ {A,B,C,D,X,Y};
      F = gateSystem(
          matrix{{A,B,C,D}},
          matrix{{X,Y}},
          matrix{{A*(X-1)^2+B}, {C*(Y+2)^2+D}}
          ); -- Define a parameter system with parameters A,B,C,D in variables X,Y
      p0 = point{{1,1,1,1}}; -- a parameter point
      sols = solveFamily(p0, F, NumberOfNodes=>3) -- 4 solutions for a fixed parameter p0

    Text
      One can also compute the permutations induced by the monodromy action.

    Example
      monodromyGroup(F,"msOptions" => {NumberOfEdges=>10})

    Text
      @BOLD "D. Numerical certification"@      

    Text
      Numerical homotopy continuation computes approximations to solutions
      of polynomial systems. An important question is whether these
      approximations correspond to actual solutions, and whether they can be
      refined to high precision.

      The package @TO "NumericalCertification"@ provides rigorous guarantees
      for numerical computations in algebraic geometry. These methods are based
      on Smale's alpha-theory and Krawczyk's method, which give criteria
      ensuring that a numerical approximation lies in a neighborhood of an
      exact solution.

      Certification can be used to verify:

      @UL {
        LI "existence of a nearby exact solution",
        LI "distinctness of computed solutions",
        LI "reality of solutions"
      }@

    Example
      needsPackage "NumericalCertification"
      R = CC[x1,x2,y1,y2];
      F = polySystem {
          3*y1 + 2*y2 -1,
          3*x1 + 2*x2 -3.5,
          x1^2 + y1^2 -1,
          x2^2 + y2^2 -1
          };
      sols = solveSystem F;
      c = certifySolutions(F, sols);
      peek c

    Text
      @BOLD "E. Numerical implicitization"@      

    Text
      The numerical methods introduced so far assume that a variety is given
      by its defining polynomial equations. In many applications, however,
      varieties arise instead as images of polynomial maps, given by
      parametrizations.

      {\it Implicitization} is the process of recovering intrinsic equations
      defining the image from its parametrization.

      In many situations, one is interested not necessarily in the full
      defining equations, but rather in basic geometric information about the
      image variety. Examples include:

      @UL {
        LI "dimension",
        LI "degree",
        LI "Hilbert function values",
        LI "membership tests for points"
      }@

      The package @TO "NumericalImplicitization"@ provides numerical methods
      for extracting such information directly from a parametrization,
      avoiding costly symbolic computations whenever possible.

      The package builds on existing numerical algebraic geometry software,
      including @TO "NumericalAlgebraicGeometry"@ and @TO "MonodromySolver"@.

    Example
      needsPackage "NumericalImplicitization"
      R = CC[m_(1,1)..m_(2,4)];
      I = ideal 0_R;
      F = (minors(2, genericMatrix(R, 2, 4)))_*;
      numericalImageDim(F, I)
      numericalImageDegree(F, I, Verbose => false)
      q = first numericalImageSample(F, I) -- sampling a point from the variety
      isOnImage(F, I, q, Verbose => false)

    Text
      Some references for numerical algebraic geometry:

      * Andrew J. Sommese, and Charles W. Wampler. "The Numerical solution of systems of polynomials arising in engineering and science." {\it World Scientific} (2005).,

      * Anton Leykin. "Numerical algebraic geometry" {\it Journal of Software for Algebra and Geometry} 3(1) (2011): 5-10.,

      * Timothy Duff, Cvetelina Hill, Anders Jensen, Kisun Lee, Anton Leykin, and Jeff Sommars. "Solving polynomial systems via homotopy continuation and monodromy." {\it IMA Journal of Numerical Analysis} 39.3 (2019): 1421-1446.,

      * Jonathan D. Hauenstein, and Frank Sottile "Algorithm 921: alphaCertified: certifying solutions to polynomial systems" {\it ACM Transactions on Mathematical Software (TOMS)} 38.4 (2012): 1-20.,

      * Michael Burr, Kisun Lee, and Anton Leykin. "Effective certification of approximate solutions to systems of equations involving analytic functions." {\it Proceedings of the 2019 International Symposium on Symbolic and Algebraic Computation}. (2019): 267-274.,

      * Justin Chen, and Joe Kileel. "Numerical implicitization." {\it Journal of Software for Algebra and Geometry} 9.1 (2019): 55-63.
     
      	    

    
      This tutorial is written by Kisun Lee.

  SeeAlso
      "NumericalAlgebraicGeometry"
      "MonodromySolver"
      "NumericalCertification"
      "NumericalImplicitization"
      "NumericalSchubertCalculus"

///
