
document {
    Key => SumsOfSquares,
    Headline => "A package for sums-of-squares problems",
    TT "SumsOfSquares", " is a package to solve sums-of-squares (SOS) problems via ",
    TO2{"SemidefiniteProgramming", "semidefinite programming"}, " (SDP). ",

    HEADER4 "Introduction",
    "Writing a polynomial as a sum of squares proves its nonnegativity for all arguments,
    but not all nonnegative polynomials are sum of squares.
    While nonnegativity of a polynomial is hard to check,
    there are efficient methods to find sums-of-squares decompositions,
    and this package makes some of them available in Macaulay2.
    These methods rely on semidefinite programming, an area from mathematical optimization.
    Several solvers (or tools) for semidefinite programming are available.
    The package ",
    TO SemidefiniteProgramming,
    " allows us to use some of these solvers in Macaulay2.
    The solver ",
    HREF {"https://github.com/coin-or/csdp/wiki","CSDP"},
    " is included in Macaulay2 and can be used without any configuration.  See ",
    TO Solver, " for how to use other solvers.",

    HEADER4 "Usage examples",
    "The most basic application is to (try to) decompose a polynomial as a sum of squares using the function ", TO "solveSOS", ".",
    EXAMPLE lines ///
      R = QQ[x,y];
      f = 2*x^4 + 2*x^3*y - 2*x^2*y^2 + 5*y^4;
      sol = solveSOS f;
    ///,
    "The return value is an object of type ", TO "SDPResult", " which, in the case of success, contains the sum-of-squares decomposition.",
    " It can be extracted with ", TO "sosPoly", ".  This returns an object of type ", TO "SOSPoly",
    ", which supports many operations that polynomials support.",
    EXAMPLE lines ///
      s = sosPoly sol
    ///,
    "The command ", TO "value(SOSPoly)", " can be used to check that the decomposition found matches the original polynomial.",
    EXAMPLE lines ///
      value(s)
    ///,

    HEADER4 "Sums of squares modulo equality constraints",
    "The package supports sum-of-squares ",
    TO2 {"solveSOS(RingElement,Matrix)","problems in quotient rings"},
    ". This can be useful to prove nonnegativity of a polynomial on a variety.  The following example is
    taken from [P05].  Consider the problem
    of proving that the polynomial ", ITALIC TEX "f = 10-x^2-y", " is nonnegative on the circle defined by ", ITALIC TEX "g = x^2 + y^2 - 1", ". ",
    "To do this we check if ", ITALIC TEX "f", " is a sum of squares in the quotient ring modulo ", ITALIC TEX "g", ". ",
    "For such a computation, a degree bound must be given by the user.",
    EXAMPLE lines ///
        R = QQ[x,y];
        S = R/ideal(x^2 + y^2 - 1);
        f = 10-x^2-y;
        sol = solveSOS (f, 2);
        sosPoly sol
    ///,
    "The previous computation gives decomposition with three sums of squares. ",
    "See ", TO "TraceObj", " for how to obtain a decomposition with only two squares.",

    HEADER4 "Other cool stuff",
    UL {
    LI {"The package implements Hilbert's algorithm to decompose a nonnegative ternary form into a sum of squares of rational functions: ", TO "sosdecTernary", "."},
    LI {"Sums of squares problems can be solved parametrically: ", TO "solveSOS", "."},
    LI {"Optimization over varieties can run using ", TO "lowerBound", "."},
    },

    HEADER4 "On the role of the coefficient field",
    "The ", TT "SumsOfSquares", " package interfaces tries to hide
    some of the difficulties that arise from using these numerical procedures. ",
    "See ", TO "coefficient field", " for more information.",

    HEADER4 "Literature",
    UL {
    LI {"[BPT12] ", EM "Semidefinite Optimization and Convex Algebraic Geometry", " SIAM Textbook, edited by G. Blekherman, P. Parrilo, and R. Thomas, (2012)."},
    LI {"[P05] ", EM "Exploiting Algebraic Structure in Sum of Squares Programs", " P. Parrilo in ", EM"Positive polynomials in control", " (2005)."},
    LI {"[PP] ", EM "Computing sum of squares decompositions with rational coefficients", " H. Peyrl and P. Parrilo ", " in Theoretical Computer Science 409 (2008) p. 269–281."},
    }
    }

doc /// --coefficient field
    Key
        "coefficient field"
    Headline
        the role of the coefficient field
    Description
      Text
        The SumsOfSquares package works with two coefficient rings: the rational numbers $\QQ$ and the real numbers $\RR$.
        Almost all operations in this package rely on a numerical semidefinite programming @TO Solver@.
        When calling such a solver, even if the input was a polynomial with rational coefficients, the result is numerical.
        The package makes some effort to round and return a rational result, but this can fail, independent of whether a rational sum-of-squares decomposition exists or not.
        In case of failure, a real result is returned.
        The following example of Scheiderer is a sum of squares, but does not admit any rational decomposition.  Consequently the package must return a real solution.
      Example
        f = library("Scheiderer", QQ[x,y,z])
        sol = solveSOS (f);
        sosPoly sol
      Text
        Given a sum-of-squares decomposition with real coefficients, it is often useful to ignore the squares with very small coefficients.
        The function @TO (clean,RR,SOSPoly)@ removes the squares whose coefficients are smaller than a given tolerance.
      Example
        clean (0.001, sosPoly sol)
    SeeAlso
        RoundTol
///

--###################################
-- SOSPoly
--###################################

doc /// --SOSPoly
    Key
        SOSPoly
        (ring, SOSPoly)
        (gens, SOSPoly)
        (coefficients, SOSPoly)
        (length, SOSPoly)
        (net, SOSPoly)
        (substitute, SOSPoly, Ring)
        (expression, SOSPoly)
        (symbol +, SOSPoly, SOSPoly)
        (symbol *, SOSPoly, SOSPoly)
        (symbol *, Number, SOSPoly)
        (symbol ^, SOSPoly, ZZ)
        (symbol ==, SOSPoly, SOSPoly)
        (symbol ==, SOSPoly, RingElement)
        (symbol ==, RingElement, SOSPoly)
        (symbol ==, SOSPoly, Matrix)
        (symbol ==, Matrix, SOSPoly)
    Headline
        A type to store sums-of-squares polynomials
    Description
      Text
        A polynomial $f\in K[x]$ is a sum of squares (SOS) if it can be written as
        $$f = \sum_i d_i g_i^2,$$
        where the $g_i$ are polynomials in $K[x]$ and the $d_i$ are positive weights in $K$.
        This data type stores polynomials as sums of squares.
        The type is a hash table consisting of the polynomials to be
        squared and summed (the 'generators'), corresponding coefficients,
        and the base ring.
        We can construct an object of type SOSPoly from the result of sum-of-squares problem.
        See @TO SDPResult@ for more on this.
      Example
        R = QQ[x,y];
        f = 2*x^4+5*y^4-2*x^2*y^2+2*x^3*y;
        sol = solveSOS f;
        sosPoly sol
      Text
        One can also construct an object of type SOSPolys by specifying all their ingredients.
      Example
        R = QQ[x,y];
        s = sosPoly(R, {x+1,y}, {2,3} )
        peek s
      Text
        The ingredients of a sum of squares can be recovered using the expected commands.
      Example
        gens s
        ring s
        coefficients s
      Text
        The length of a sum-of-squares polynomial is the number of squares.
      Example
        length s
      Text
        Sums of squares support many common operations with polynomials.
      Example
        2 * s
        s + s
        s * s
        s == s
      Text
        The actual polynomial can be recovered using @TO(value,SOSPoly)@.
      Example
        value s
      Text
        We can also produce a formal @TO Expression@.
      Example
        expression s
      Text
        Objects of type @TO SOSPoly@ support the @TO substitute@ command.
        This cannot be used to change the coefficient field, though.
        See @TO "coefficient field"@ for some of the limitations.
      Example
        S = QQ[x,y,z];
        sub (s, S)
    SeeAlso
        sosPoly
///


doc /// -- SDPResult
    Key
       SDPResult
       (net, SDPResult)
       (status, SDPResult)
       Status
       GramMatrix
       MomentMatrix
    Headline
       result of a semidefinite programming computation
    Description
      Text
        This type encapsulates the result of a semidefinite programming computation.
      Example
        R = QQ[x][t];
        f = x^2 - 3*x - t;
        sol = solveSOS (f, -t, RoundTol=>12)
        peek sol
      Text
        The fields can be extracted with the operator @TO symbol #@.
      Example
        sol#GramMatrix
      Text
        If the Gram matrix is different from null, then the sum-of-squares polynomial can be recovered with @TO sosPoly@.
      Example
        sosPoly sol
///

--###################################
-- Methods
--###################################

doc /// --cleanSOS
    Key
        (clean,RR,SOSPoly)
    Headline
        remove squares with very small coefficients from a sum of squares
    Usage
        clean (tol, s)
    Inputs
      tol:RR
        the tolerance for the coefficients.
      s:SOSPoly
    Outputs
        :SOSPoly
          a cleaned up @TO SOSPoly@
    Consequences
    Description
      Text
        Given a @TO2 {SOSPoly,"sum-of-squares"}@ polynomial with real coefficients in the reals,
        this method removes the squares with coefficients smaller than the given tolerance.
        It does nothing on inputs with rational coefficients.
      Example
        R = RR[x,y];
        s = sosPoly(R, {x^2+.0001*x+1, y}, {2, .0001})
        clean( .001, s )
      Code
      Pre
    SeeAlso
        SOSPoly
///

doc /// --value
    Key
        (value,SOSPoly)
    Headline
        expansion of a weighted SOS decomposition
    Usage
        value(s)
    Inputs
        s:SOSPoly
    Outputs
        :RingElement
          a polynomial
    Consequences
    Description
      Text
        Given an object of type @TO SOSPoly@,
        this method expands the polynomial and computes $f = \sum_i d_i g_i^2$.
      Example
        R = QQ[x,y];
        s = sosPoly(R, {x+1,y}, {2,3} )
        value( s )
      Code
      Pre
    SeeAlso
        (sosPoly,Matrix,Matrix)
///

doc /// --sosPoly
    Key
        sosPoly
        (sosPoly,Ring,List,List)
        (sosPoly,List,List)
        (sosPoly,Matrix,Matrix)
        (sosPoly,SDPResult)
    Headline
        make an SOS polynomial
    Usage
        s = sosPoly (SDPR)
        s = sosPoly (mon,Q)
        s = sosPoly (R,polys,coeffs)
    Inputs
        SDPR:SDPResult
          the result of an SDP computation
        Q:Matrix
          positive semidefinite (Gram matrix)
        mon:Matrix
          a vector of monomials
        R:Ring
        polys:List
          of polynomials
        coeffs:List
          of scalars
    Outputs
        s:SOSPoly
    Consequences
    Description
      Text
        This method creates an object of type @TO SOSPoly@.
        Very often this is applied to an object of type @TO SDPResult@, the result of a semidefinite programming computation.
      Example
        R = QQ[x,y];
        f = 2*x^4+5*y^4-2*x^2*y^2+2*x^3*y;
        sosPoly solveSOS f
      Text
        One can also input a Gram matrix $Q$ and a vector of monomials $mon$.
      Example
        Q = matrix(QQ,{{1,1,1},{1,1,1},{1,1,1}});
        mon = matrix{{1},{x},{y}};
        sosPoly(mon,Q)
      Text
        Alternatively, a sum-of-squares polynomial can be created from a list of generators and weights.
      Example
        s = sosPoly(R, {x+1,y}, {2,3} )
      Code
      Pre
    SeeAlso
        solveSOS
        Solver
///

doc /// --solveSOS
    Key
        solveSOS
        (solveSOS,RingElement)
        (solveSOS,RingElement,RingElement)
    Headline
        solve a sum-of-squares problem
    Usage
        solveSOS(f)
        solveSOS(f,objFun)
    Inputs
        f:RingElement
          a polynomial
        objFun:RingElement
          a linear function of the parameters (optional)
    Outputs
        :SDPResult
    Consequences
    Description
      Text
        This method solves sums-of-squares problems.
        Given a rational (or real) polynomial $f(x)$, it attempts to find a rational (or real)
        positive semidefinite matrix $Q$ and a vector of monomials $mon$ such that
        $$f(x) = mon' Q mon.$$
        The algorithm first computes a floating point solution,
        and then tries to obtain an exact solution by rounding the numerical result.
        If the rounding fails, the numerical solution is returned.
      Example
        R = QQ[x,y];
        f = 2*x^4+5*y^4-2*x^2*y^2+2*x^3*y;
        sol = solveSOS f;
        Q = sol#GramMatrix
        mon = sol#Monomials
        transpose(mon)*Q*mon - f
      Text
        {\bf SOS with parameters:}
        If the coefficients of the polynomial are linearly parametrized, we can search for parameters which render a polynomial to be a sum of squares.
        In the following example, the variable $t$ will be treated as a free parameter.
      Example
        R = QQ[x][t];
        f = (t-1)*x^4+1/2*t*x+1;
        sol = solveSOS f;
        sosPoly sol
        sol#Parameters
      Text
        It is possible to solve sums-of-squares problems with several parameters.
        In the following example we increase two of the coefficients of the Robinson polynomial until it becomes a sum of squares.
      Example
        R = QQ[x,y,z][s,t]
        g = library("Robinson", {x,y,z}) + s*x^6 + t*y^6
        sol = solveSOS g;
        sol#Parameters
      Text
        {\bf SOS with parameter optimization:}
        The method also allows to optimize a linear function of the parameters.
        More precisely, given a polynomial $f(x;p)$ that depends affinely on some parameters $p$, we can solve the problem

        $$min_{p} \, objFun(p) \,\,\, s.t. \,\,\, f(x; p) \, is SOS $$

        In the following example we minimize $-t$ in order to find a lower bound for the polynomial $x^2-3x$:
      Example
        R = QQ[x][t];
        f = x^2 - 3*x - t;
        sol = solveSOS (f, -t, RoundTol=>12);
        sol#Parameters
      Text
        By default the method tries to obtain rational values of the parameters.
        Since there is a trade-off between rounding and optimality, we specify the @TO2 {RoundTol,"rounding precision"}@ as an optional input argument.
      Code
      Pre
    SeeAlso
        (solveSOS,RingElement,Matrix)
        sosPoly
        SDPResult
        Solver
///

doc /// --solveSOS (quotient ring)
    Key
        (solveSOS,RingElement,RingElement,Matrix)
        (solveSOS,RingElement,Matrix)
        (solveSOS,RingElement,ZZ)
        (solveSOS,RingElement,RingElement,ZZ)
    Headline
        sum-of-squares problem in a quotient ring
    Usage
        solveSOS(f,mon)
        solveSOS(f,objFun,mon)
        solveSOS(f,D)
        solveSOS(f,objFun,D)
    Inputs
        f:RingElement
          a polynomial
        objFun:RingElement
          a linear function of the parameters (optional)
        mon:Matrix
          a vector of monomials (alternatively a degree bound D)
    Outputs
        :SDPResult
    Consequences
    Description
      Text
        This method allows to compute sums-of-square decompositions in quotient rings.
        A vector of monomials must be provided in this case.
      Example
        R = QQ[x,y]/ideal(x^2 + y^2 - 1);
        f = 10-x^2-y;
        mon = matrix {{1}, {x}, {y}};
        solveSOS (f, mon)
      Text
        If a degree bound $D$ is given, the method will use the vector of monomials of degree at most $D/2$.
      Example
        solveSOS (f, 2)
      Text
        Parametrized sums-of-squares problems can also be solved in quotient rings.
      Example
        S = R[t];
        solveSOS(f-t,-t,mon,RoundTol=>12)
      Text
        {\bf Caveat}
      Text
        The @TO2 {"coefficient field","rational rounding"}@ may also fail in quotient ring computations.
        In this case, the object @TO SOSPoly@ constructed from the object
        @TO SDPResult@ can live in a newly created ring, instead of the quotient that one started with.
      Example
        R = QQ[x,y]/ideal(x^2 + y^2 - 1);
        f = 10-x^2-y;
        mon = matrix {{1}, {x}, {y}};
        s = solveSOS (f, mon, RoundTol=>infinity);
        ring sosPoly s
      Text
        The above ring is not a quotient ring.
        One can construct a new quotient ring and work there.
        However, this will only work reliably for principal ideals,
        as otherwise the Gröbner basis engine might fail due to inexact computations.
      Example
        R' = ring sosPoly s;
        S = R'/(sub (ideal (x^2 + y^2 - 1), R'))
        sub (f, S) == sub (sosPoly s, S)
      Code
      Pre
    SeeAlso
        solveSOS
///

doc /// --sosdecTernary
    Key
        sosdecTernary
        (sosdecTernary, RingElement)
    Headline
       sum of squares decomposition for ternary forms
    Usage
        (p,q) = sosdecTernary(f)
    Inputs
        f:RingElement
          a homogeneous polynomial in 3 variables
    Outputs
        p:List
          of @TO SOSPoly@s
        q:List
          of @TO SOSPoly@s
    Consequences
    Description
      Text
        Given a nonnegative ternary form $f$, this method uses Hilbert's
        algorithm to compute a decomposition of
        $f$ as $f=\frac{\prod_ip_i}{\prod_iq_i}$ where each factor $p_i$
        and $q_i$ is a sum of squares.
        The method returns null if $f$ is not nonnegative.
        As an example, consider the homogeneous Motzkin polynomial.
      Example
        R = RR[x,y,z];
        f = library ("Motzkin", {x,y,z});
        (p,q) = sosdecTernary (f);
      Text
        The result, in this case, is a quotient of two sums of squares.
      Example
        (#p, #q)
        clean(1e-4, f * value q#0 - value p#0)
      Text
        {\bf References:}
        {\it Products of positive forms, linear matrix inequalities, and Hilbert 17th problem for ternary forms}, E. de Klerk, and D.V. Pasechnik, European J. Oper. Res. (2004), pp. 39-45.
    Caveat
        $\bullet$
        This implementation only works with the @TO2 {Solver,"solvers"}@ "CSDP" and "MOSEK".

        $\bullet$
        Due to the iterative nature of the algorithm, it could happen that some of the output sums of squares are rational while some are real.
///

doc /// --sosInIdeal
    Key
        sosInIdeal
        (sosInIdeal, Matrix, ZZ)
        (sosInIdeal, Ring, ZZ)
    Headline
        sum of squares polynomial in ideal
    Usage
        (sol,mult) = sosInIdeal(h,D)
        sol = sosInIdeal(R,D)
    Inputs
        h:Matrix
          row vector with polynomial entries
        D:ZZ
          bound on the degree of the SOS polynomial
        R:QuotientRing
          a quotient of a polynomial ring
    Outputs
        sol:SDPResult
        mult:Matrix
          column vector with polynomial multipliers
    Consequences
    Description
      Text
        This methods finds sums-of-squares polynomials in ideals.
        It accepts two types of inputs that are useful for different purposes.
        The first invocation is to give a one row matrix with polynomial entries and a degree bound.
        The method then tries to find a sum of squares in the generated ideal.
        More precisely, given equations $h_1(x),...,h_m(x)$, the method looks for polynomial multipliers $h_i(x)$ such that $\sum_i l_i(x) h_i(x)$ is a sum of squares.
     Example
        R = QQ[x,y,z];
        h = matrix {{x^2-4*x+2*y^2, 2*z^2-y^2+2}};
        (sol,mult) = sosInIdeal (h, 2);
        sosPoly sol
        h * mult == sosPoly sol
     Text
        The second invocation is on a quotient ring, also with a degree bound.
        This tries to decompose the zero of the quotient ring as a sum of squares.
     Example
        S = R/ideal h;
        sol = sosInIdeal (S, 2);
        sosPoly sol
        sosPoly sol
    Caveat
        This implementation only works with the @TO2 {Solver,"solvers"}@ "CSDP" and "MOSEK".
///

doc /// --lowerBound
    Key
        lowerBound
        (lowerBound, RingElement)
        (lowerBound, RingElement, ZZ)
        (lowerBound, RingElement, Matrix, ZZ)
    Headline
        finds a lower bound for a polynomial
    Usage
        (bound,sol) = lowerBound(f)
        (bound,sol) = lowerBound(f,D)
        (bound,sol,mult) = lowerBound(f,h,D)
    Inputs
        f:RingElement
          a polynomial or a rational function
        D:ZZ
          degree bound for the SDP relaxation (optional)
        h:Matrix
          row vector with polynomial entries (optional)
    Outputs
        bound:
          a lower bound on f
        sol:SDPResult
        mult:Matrix
          column vector with polynomial multipliers
    Consequences
    Description
      Text
        This method finds a lower bound for a polynomial or rational function $x\mapsto f(x)$.
        More precisely, this method solves the following relaxation

        $$max \, t \,\,\, s.t. \,\,\, f(x) - t \, is SOS $$

        In some cases the minimizer can be extracted with the method @TO recoverSolution@.
      Example
        R=QQ[x];
        f = (x-1)^2 + (x+3)^2;
        (bound,sol) = lowerBound(f);
        (bound, recoverSolution sol)
        f - bound == sosPoly sol
      Text
        By default the method tries to obtain a rational lower bound.
        Since there is a trade-off between rounding and optimality, we specify the @TO2 {RoundTol,"rounding precision"}@ as an optional input argument.
      Text
        {\bf Quotient rings:}
        Given an ideal $I$, we can also find a lower bound for $f$ on the variety of $I$.
        This can be done by constructing the associated quotient ring.
        A degree bound must be provided.
      Example
        R = QQ[x,y]/ideal(x^2 - x, y^2 - y);
        f = x - y;
        (bound,sol) = lowerBound(f,2);
        bound
        f - bound == sosPoly sol
      Text
        {\bf Avoiding quotient rings:}
        Constructing the quotient ring is sometimes too expensive since it requires Gröbner bases.
        There is an alternative (though weaker) relaxation that avoids Gröbner bases computation.
        Given equations $h_1(x),...h_m(x)$, we can look for multipliers $l_i(x)$ such that $f(x) - t + \sum_i l_i(x) h_i(x)$ is a sum of squares.
      Example
        R = QQ[x,y];
        f = x - y;
        h = matrix{{x^2 - x, y^2 - y}};
        (bound,sol,mult) = lowerBound (f, h, 2);
        bound
        f - bound + h*mult == sosPoly sol
      Text
        {\bf Optimizing rational functions:}
        The following is an example of how to optimize a rational function.
      Example
        R = QQ[x];
        f = (x^2-x)/(x^2+1);
        (bound,sol) = lowerBound (f);
        (bound, recoverSolution sol)
    SeeAlso
        recoverSolution
///

doc /// --recoverSolution
    Key
        recoverSolution
        (recoverSolution,Matrix,Matrix)
        (recoverSolution,SDPResult)
    Headline
        factor a rank one positive semidefinite matrix
    Usage
        sol = recoverSolution(mon,X)
        sol = recoverSolution(sdpsol)
    Inputs
        mon:Matrix
          of monomials
        X:Matrix
          the moment matrix
        sdpsol:SDPResult
    Outputs
        sol:List
          the optimal solution
    Consequences
    Description
      Text
        This method attempts to find the optimizing solutions of a sum-of-squares problem
        by checking if the moment matrix is rank one.  In this case it
        factorizes the matrix and returns the defining vector.
      Example
        R = RR[x,y];
        mon = matrix {{1},{x},{y}};
        X = matrix(RR, {{1,0,1},{0,0,0},{1,0,1}} );
        sol = recoverSolution(mon,X)
      Text
        See @TO lowerBound@ for how to use it to find a point realizing a
        lower bound.
      Code
      Pre
    SeeAlso
        solveSOS
        lowerBound
///

doc /// --checkSolver
    Key
        checkSolver
        (checkSolver,String)
        (checkSolver,String,String)
        (checkSolver,String,Function)
    Headline
        tests a semidefinite programming solver
    Usage
        checkSolver(solver)
        checkSolver(solver,fun)
    Inputs
        solver:String
          either "M2" or "CSDP" or "SDPA" or "MOSEK"
        fun:Function
          optional, one of @TO optimize@, @TO solveSOS@, @TO sosdecTernary@, @TO sosInIdeal@, or @TO lowerBound@
    Consequences
    Description
      Text
        This method tests whether various functions work properly using a specified solver.
        The most basic invocation uses only the name of the solver.
        This runs all tests with that solver and prints a summary.
        All tests should pass for CSDP and MOSEK.
      Code
      Pre
    SeeAlso
        Solver
///

doc /// --library
    Key
        library
        (library,String,List)
        (library,String,Ring)
    Headline
        library of interesting nonnegative forms
    Usage
        library(name,R)
        library(name,var)
    Inputs
        name:String
          either "Motzkin", "Robinson", "Schmuedgen", "Lax-Lax", "Choi-Lam", "Scheiderer", "Harris"
        R:Ring
          a polynomial ring
        var:List
          of variables
    Outputs
        :RingElement
          a nonnegative form
    Consequences
    Description
      Text
        This method contains a library of some interesting nonnegative forms.
      Text
        The Motzkin polynomial is a ternary sextic that is nonnegative, but is not a sum of squares.
        It was the first such example found.
      Example
        R = QQ[x,y,z];
        library("Motzkin", R)
      Text
        The Robinson and Schmüdgen polynomials are also ternary sextics that are not sums of squares.
      Example
        library("Robinson", R)
        library("Schmuedgen", R)
      Text
        The Lax-Lax and Choi-Lam polynomials are quaternary quartics that are not sums of squares.
      Example
        R = QQ[x,y,z,w];
        library("Lax-Lax", R)
        library("Choi-Lam", R)
      Text
        The Scheiderer polynomial is a sum of squares over the reals, but not over the rationals.
      Example
        R = QQ[x,y,z];
        library("Scheiderer", R)
      Text
        The Harris polynomial is a ternary form of degree 10 with 30 projective zeros (the largest number known in August 2018).
      Example
        library("Harris", R)
      Text
        {\bf References:}
        {\it Some concrete aspects of Hilbert's 17th problem}. B. Reznick. Contemporary mathematics (2000), 253, pp. 251-272.
      Code
      Pre
    SeeAlso
///

--###################################
-- Unexported methods (for developers)
--###################################

-- doc /// --createSOSModel
--     Key
--         createSOSModel
--         (createSOSModel,RingElement,Matrix)
--     Headline
--         space of Gram matrices of a polynomial (for developers)
--     Usage
--         (C,Ai,Bi,A,B,b) = createSOSModel(f,mon)
--     Inputs
--         f:RingElement
--           a polynomial
--         mon:Matrix
--           a vector of monomials
--     Outputs
--         C:Matrix
--         Ai:Sequence
--         Bi:Sequence
--         A:Matrix
--         B:Matrix
--         b:Matrix
--     Consequences
--     Description
--       Text
--         This method creates the kernel and image model of the Gram matrices of a polynomial $f$.
-- 
--         A Gram matrix representation of $f$ is a symmetric matrix $Q$ such that
--         $f = mon' Q mon$,
--         where $mon$ is a vector of monomials.
--         The set of all Gram matrices $Q$ is an affine subspace.
--         This subspace can be described in image form as
--         $Q = C - \sum_i y_i A_i$,
--         or in kernel form as
--         $A q = b$
--         where $q$ is the @TO2 {smat2vec,"vectorization"}@ of $Q$.
-- 
--         For parametric sum-of-squares problems the image form is
--         $Q = C - \sum_i y_i A_i - \sum_j p_j B_j$,
--         where $p_j$ are the parameters,
--         and the kernel form is
--         $A q + B p = b$.
--       Code
--       Pre
--     SeeAlso
-- ///

--###################################
-- Symbols
--###################################

doc /// -- RoundTol
    Key
        RoundTol
        [solveSOS,RoundTol]
        [lowerBound,RoundTol]
        [sosdecTernary,RoundTol]
        [sosInIdeal,RoundTol]
    Headline
        tolerance for rational rounding
    Consequences
    Description
      Text
        The optional argument {\tt RoundTol} specifies the minimal rounding precision in $d$ binary digits.

        Sums-of-squares problems are solved numerically using a semidefinite programming solver, and afterwards the package attempts to round the floating point solution to rational numbers.
        The rounding strategy is guaranteed to work whenever the space of Gram matrices is full dimensional.
        For optimization problems the rounding may cause a loss in optimality.
        The argument {\tt RoundTol} allows to control the trade-off between optimality and simplicity.
        Higher values of {\tt RoundTol} lead to better solutions.
      Example
        R = QQ[x,z];
        f = x^4+x^2+z^6-3*x^2*z^2;
        (bound,sol) = lowerBound (f,RoundTol=>4);
        bound
        (bound,sol) = lowerBound (f,RoundTol=>12);
        bound
      Text
        One can also skip the rounding by setting {\tt RoundTol => infinity}.
      Example
        (bound,sol) = lowerBound (f,RoundTol=>infinity);
        bound
      Text
        {\bf References:}
        {\it Computing sum of squares decompositions with rational coefficients}, H. Peyrl and P. Parrilo, in Theoretical Computer Science 409 (2008) p. 269–281.
      Code
      Pre
    SeeAlso
        roundPSDmatrix
///

document { --Verbosity
    Key => {
        [solveSOS,Verbosity],
        [sosdecTernary,Verbosity],
        [sosInIdeal,Verbosity],
        [lowerBound,Verbosity],
        [checkSolver,Verbosity],
        },
    Headline => "control the level of information printed",
    "This optional argument indicates how much information should be given to the user.  The possible values are:",
    UL{
      {"0 (default)", " -- no information is printed."},
      {"1", " -- minimal information (solver used, input/output files, status of the solver)."},
      {"2", " -- more detailed information (e.g., progress of the solver)."},
      },
    BR{},
    }

doc /// --TraceObj
    Key
        TraceObj
        [solveSOS,TraceObj]
    Headline
        whether to use trace as the objective function
    Consequences
    Description
      Text
        Using the trace as the objective function is a heuristic for obtaining sum-of-squares decompositions with small number of squares.
        Here we repeat Example 5 from [P05] and recover the shorter solution from that paper.
      Example
        R = QQ[x,y]/ideal(x^2 + y^2 - 1);
        f = 10-x^2-y;
        sosPoly solveSOS (f, 2)
        sosPoly solveSOS (f, 2, TraceObj=>true)
      Code
      Pre
    SeeAlso
///


doc /// -- the Solver option is documented in the semidefinite programming documentation
    Key
      [solveSOS,Solver]
      [sosdecTernary,Solver]
      [sosInIdeal,Solver]
      [lowerBound,Solver]
    Headline
      picking a semidefinite programming solver
    Description
      Text
        Macaulay2 has a powerful solver already configured.  If necessary you can use this variable to
        change the semidefinite programming solver used in the background.  See @TO Solver@ for details.
///
