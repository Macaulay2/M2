
doc /// --SemidefiniteProgramming
    Key
        SemidefiniteProgramming
    Headline
        A package for solving semidefinite programs
    Description
      Text
        This is a package for solving semidefinite programming (SDP) problems.

        Given symmetric matrices $C, A_i$ and a vector $b$, the primal SDP problem is

        $$min_{X} \, C \bullet X \,\,\, s.t. \,\,\, A_i \bullet X = b_i \, and \, X \geq 0$$

        and the dual SDP problem is

        $$max_{y,Z} \, \sum_i b_i y_i \,\,\, s.t. \,\,\, Z = C - \sum_i y_i A_i \, and \, Z \geq 0$$

        We can construct a semidefinite program using the method @TO sdp@.
      Example
        P = sdp(matrix{{1,0},{0,2}}, matrix{{0,1},{1,0}}, matrix{{-1}})
      Text
        The semidefinite program can be solved numerically using the method @TO optimize@.
      Example
        (X,y,Z,stat) = optimize P;
        (X,y)
      Text
        See @TO Solver@ for a discussion of the available SDP solvers.
        The method @TO2 {(refine,SDP,Sequence),"refine"}@ can be used to improve the precision of the solution.

        In small cases it is possible to solve the SDP symbolically, by forming the ideal of @TO2 {criticalIdeal,"critical equations"}@.
      Example
        (I,X,y,Z) = criticalIdeal P;
        radical I
      Code
      Pre
    SeeAlso
///

--###################################
-- Methods
--###################################

doc /// --sdp
    Key
        SDP
        sdp
        (sdp,Matrix,Sequence,Matrix)
        (sdp,Matrix,Matrix,Matrix)
        (sdp,List,Matrix,RingElement)
        (ring,SDP)
    Headline
        construct a semidefinite program
    Usage
        P = sdp(C,A,b)
        P = sdp(var,M,objFun)
    Inputs
        C:Matrix
          a symmetric $n\times n$ matrix
        A:Sequence
          consisting of $m$ symmetric $n\times n$ matrices
        b:Matrix
          a $m\times 1$ matrix
        var:List
          of variables
        M:Matrix
          constraint matrix with affine-linear entries
        objFun:RingElement
          linear function to be minimized
    Outputs
        P:SDP
          a semidefinite programming problem
    Consequences
    Description
      Text
        A semidefinite program (SDP) is defined by a some symmetric matrices $C, A_i$ and a vector $b$.
        The SDP has a primal and a dual problem.
        The primal problem is

        $$min_{X} \, C \bullet X \,\,\, s.t. \,\,\, A_i \bullet X = b_i \, and \, X \geq 0$$

        and the dual problem is

        $$max_{y,Z} \, \sum_i b_i y_i \,\,\, s.t. \,\,\, Z = C - \sum_i y_i A_i \, and \, Z \geq 0$$
      Text
        The type @TO SDP@ stores semidefinite programs.
        There are two ways to construct an SDP.
        The first option is to provide the matrices $C,A_i,b$.
      Example
        P = sdp(matrix{{1,0},{0,2}}, matrix{{0,1},{1,0}}, matrix{{-1}})
      Text
        The second option is to provide a matrix $M(v)$, with affine entries, and a linear function $f(v)$.
        This constructs an SDP in dual form: minimize $f(v)$ subject to $M(v)\geq 0$.
      Example
        R = QQ[u,v,w];
        M = matrix {{1,u,3-v},{u,5,w},{3-v,w,9+u}}
        objFun = u+v+w;
        P = sdp({u,v,w}, M, objFun);
      Text
        Semidefinite programs can be solved numerically using the method @TO optimize@, and in small cases also symbolically with the method @TO criticalIdeal@.
      Code
      Pre
    SeeAlso
///

doc /// --roundPSDmatrix
    Key
        roundPSDmatrix
    Headline
        rational rounding of a PSD matrix
    Usage
        (Qp,ispsd) = roundPSDmatrix(Q,A,b,d)
    Inputs
        Q:Matrix
          a symmetric matrix (real)
        A:Matrix
          (rational)
        b:Matrix
          (rational)
        d:ZZ
          the rounding precision
    Outputs
        Qp:Matrix
          the rounded matrix (rational)
        ispsd:Boolean
          true if Qp is positive semidefinite
    Consequences
    Description
      Text
        Let $S^n$ be the space of symmetric $n\times n$ matrices,
        and let $L \subset S^n$ be a rational affine subspace.
        By @TO2 {smat2vec,"vectorization"}@ we may describe this subspace in the form  $A q = b$ for some matrix $A$ with $n(n+1)/2$ columns.
        Given a real matrix $Q\in S^n$, this method finds a nearby rational matrix $Q_p$ on $L$.
      Code
      Pre
    SeeAlso
        smat2vec
///

doc /// --smat2vec
    Key
        smat2vec
        (smat2vec,Matrix)
        (smat2vec,List)
        vec2smat
        (vec2smat,Matrix)
        (vec2smat,List)
        Scaling
     	[smat2vec,Scaling]
    Headline
        vectorization of a symmetric matrix
    Usage
        v = smat2vec A
        A = vec2smat v
    Inputs
        A:Matrix
          symmetric
        v:Matrix
          a vector
    Outputs
        v:Matrix
        A:Matrix
    Consequences
    Description
      Text
        The method {\tt smat2vec} obtains the vectorization of a symmetric matrix.
        The method {\tt vec2smat} performs the reverse operation.
      Example
        A = matrix(QQ, {{1,2,3,4},{2,5,6,7},{3,6,8,9},{4,7,9,10}})
        v = smat2vec A
        vec2smat v
      Text
        The scaling of the off-diagonal entries can be controlled with the optional parameter.
      Example
        smat2vec(A,Scaling=>2)
      Code
      Pre
    SeeAlso
///

doc /// --PSDdecomposition
    Key
        PSDdecomposition
    Headline
        factorization of a positive semidefinite matrix
    Usage
        (L,D,P) = PSDdecomposition A
    Inputs
        A:Matrix
    Outputs
        L:Matrix
          nonsingular
        D:Matrix
          diagonal
        P:Matrix
          permutation matrix
    Consequences
    Description
      Text
        Given a positive semidefinite matrix $A$, this method factorizes it in the form $P' A P = L D L'$,
        where $P$ is a permutation matrix,
        $L$ is nonsingular,
        $D$ is diagonal.
        If $A$ is a real matrix, this factorization is obtained from its eigenvalue decomposition.
        For rational matrices we use the LDL decomposition [Golub-vanLoan'89].
        The method returns null if $A$ is not positive semidefinite.
      Example
        A = matrix(QQ, {{5,3,5},{3,2,4},{5,4,10}})
        (L,D,P) = PSDdecomposition(A)
        L*D*transpose(L) == transpose(P)*A*P
      Text
        {\bf References:}
        {\it Matrix Computations}, Gene Golub and Charles van Loan. Johns Hopkins
        series in the Mathematical Science (1989), 2 ed., pp. 133-148.
      Code
      Pre
    SeeAlso
///

doc /// --optimize
    Key
        optimize
        (optimize,SDP)
        (optimize,SDP,Matrix)
    Headline
        solve a semidefinite program
    Usage
        (X,y,Z,stat) = optimize P
        (X,y,Z,stat) = optimize(P,y0)
    Inputs
        P:SDP
          a semidefinite programming problem
        y0:Matrix
          a dual strictly feasible matrix (optional)
    Outputs
        X:
          an $n\times n$ matrix, primal variable (not available if Solver=>"M2")
        y:
          an $m\times 1$ matrix, dual variable
        Z:
          an $n\times n$ matrix, dual variable
        stat:String
          the status of the SDP solver
    Consequences
    Description
      Text
        This method solves a semidefinite programming problem.
        There is an interface to the @TO2 {[optimize,Solver],"solvers"}@ CSDP, SDPA and MOSEK.
        The default solver is CSDP, which is preinstalled with Macaulay2.
        Alternatively, there is rudimentary dual interior point method implemented entirely in Macaulay2 language.
      Example
        P = sdp(matrix{{1,0},{0,2}}, matrix{{0,1},{1,0}}, matrix{{-1}});
        (X,y,Z,stat) = optimize P;
        y
      Text
        {\bf References:}
        {\it Convex Optimization}, Boyd, Vandenberghe, Cambridge University Press (2004), pp. 618-619, pp. 463-466.
      Code
      Pre
    Caveat
        The "M2" solver might fail if the dual problem is not strictly feasible.
        It also does not return the primal solution.
    SeeAlso
        (refine,SDP,Sequence)
///

doc /// --criticalIdeal
    Key
        criticalIdeal
        (criticalIdeal,SDP,ZZ)
        (criticalIdeal,SDP)
    Headline
        ideal of critical equations of a semidefinite program
    Usage
        (I,X,y,Z) = criticalIdeal(P)
        (I,X,y,Z) = criticalIdeal(P,rk)
    Inputs
        P:SDP
          a semidefinite programming problem
        rk:ZZ
          the rank of the primal matrix (optional)
    Outputs
        I:Ideal
          of critical equations
        X:
          an $n\times n$ matrix, primal variable
        y:
          an $m\times 1$ matrix, dual variable
        Z:
          an $n\times n$ matrix, dual variable
    Consequences
    Description
      Text
        This method computes the ideal of critical equations of an SDP.
        This ideal can be used to solve the SDP symbolically.
      Example
        A = (-matrix{{0,1,0},{1,0,0},{0,0,1}}, matrix{{0,0,1},{0,0,0},{1,0,0}}, -matrix{{0,0,0},{0,0,1},{0,1,0}});
        (C, b) = (matrix{{1/1,0,3},{0,5,0},{3,0,9}}, matrix{{-1},{-1},{-1}});
        P = sdp(C,A,b);
        (I,X,y,Z) = criticalIdeal P;
        degree I
      Text
        We can restrict the rank of the primal matrix X.
      Example
        rk = 1;
        (J,X,y,Z) = criticalIdeal(P, rk);
        degree J
      Code
      Pre
    SeeAlso
///

doc /// --refine
    Key
        (refine,SDP,Sequence)
    Headline
        refine an SDP solution
    Usage
        (X1,y1) = refine(P,(X0,y0))
    Inputs
        P:SDP
          a semidefinite programming problem
        X0y0:
          primal and dual solutions
    Outputs
        X1y1:
          refined primal and dual solutions
    Consequences
    Description
      Text
        This function uses Newton's method to improve the precision of an optimal primal/dual pair.
        The refined solution has relative error bounded by min(@TO ErrorTolerance@,2(-@TO Bits@)). 
        The number of iterations made is at most @TO Iterations@.
      Example
        P = sdp(matrix{{1,0},{0,2}}, matrix{{0,1},{1,0}}, matrix{{-1}});
        (X0,y0) = (matrix{{.71, -.5}, {-.5, .35}}, matrix{{-1.41}})
        (X1,y1) = refine(P,(X0,y0))
      Code
      Pre
    SeeAlso
        refine
        criticalIdeal
///

--###################################
-- Unexported methods (for developers)
--###################################

doc /// --project2linspace
    Key
        project2linspace
    Headline
        project a rational point onto affine subspace
    Usage
        xp = project2linspace(A,b,x0)
    Inputs
        A:Matrix
        b:Matrix
          a vector
        x0:Matrix
          a rational vector
    Outputs
        xp:Matrix
          the projection of x0
    Consequences
    Description
      Text
        Projects a rational point $x_0$ onto the affine subspace given by $A x = b$
      Code
      Pre
    SeeAlso
///

--###################################
-- Symbols
--###################################
document { --Verbosity
    Key => {
	Verbosity,
        [optimize,Verbosity],
        [checkOptimize,Verbosity]
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

document { --Solver
    Key => {
        Solver,
        [optimize,Solver],
        },
    Headline => "picking a semidefinite programming solver",
    "There are several specialized solvers (or tools) for semidefinite programming. ",
    "This package interfaces some of these solvers to Macaulay2. ",
    "In particular, the open source solver CSDP is included with Macaulay2, and is configured as the default. ",
    "There is also a rudimentary semidefinite programming solver implemented in the Macaulay2 language, but for most applications it will be insufficient. ",
    "The package supports the following solvers: ",
    UL{
      {"\"M2\"", " -- a simple dual interior point method implemented in Macaulay2"},
       {"\"CSDP\"", " -- this is an open source solver, available at ", HREF "https://projects.coin-or.org/Csdp/" },
       {"\"SDPA\"", " -- this is an open source solver, available at ", HREF "http://sdpa.sourceforge.net/" },
       {"\"MOSEK\"", " -- this is a commercial solver, free for academic use, available at ", HREF "https://www.mosek.com/" },
      },
    "In our experience CSDP and MOSEK give the best results. ",
    "An easy way to make an additional solver available to Macaulay2 is to add the executable to the PATH environment variable. ",
    "Another way is to explicitly specify the location of the executable in the package configuration:",
    EXAMPLE lines ///
       needsPackage ("SemidefiniteProgramming", Configuration=>{"CSDPexec"=>"/some/path/csdp"});
       needsPackage ("SemidefiniteProgramming", Configuration=>{"SDPAexec"=>"/some/path/sdpa"});
       needsPackage ("SemidefiniteProgramming", Configuration=>{"MOSEKexec"=>"/some/path/mosek"});
    ///,
    "A third method is to use the function ", TO changeSolver, ". ",
    "After configuring, the method ", TO "checkOptimize", " can be used to check if a solver works.",
    BR{},
    BR{},

    "The default solver can also be specified when loading the package:",
    EXAMPLE lines ///
       needsPackage ("SemidefiniteProgramming", Configuration=>{"DefaultSolver"=>"CSDP"});
    ///,
    "If no default solver is specified, the package tries to use, in this order,
    CSDP, MOSEK, SDPA, M2.",

    HEADER2 "Saving the configuration",
    "The configuration options \"CSDPexec\", \"SDPAexec\", \"MOSEKexec\", \"DefaultSolver\" can be saved by editing the file \"init-SemidefiniteProgramming.m2\", which is located in the application directory:",
    EXAMPLE lines ///
        applicationDirectory()
    ///,
    }


doc ///
    Key
      changeSolver
    Headline
      change the SDP solver
    Usage
      changeSolver (solver, path)
    Inputs
        Solver:String
	  the name of the solver to configure: CSDP, MOSEK, or SDPA
	path:String
	  the full path to the executable of the solver
    Description
      Text
        The path can be specified as an absolute or relative path of the executable and must include the name of the executable.        
      Example
        changeSolver ("CSDP", "/path/to/csdp")
    Caveat
      The function does not check if the specified executable exists or is functional.  To find out try @TO checkOptimize@.
    SeeAlso
      Solver
///

doc ///
    Key
      checkOptimize
      (checkOptimize,String)
    Headline
      check an SDP solver
    Usage
      checkOptimize (solver)
    Inputs
      solver:String
	    the name of the solver to configure: CSDP, MOSEK, M2, or SDPA
    Outputs
      :List
        indicating whether the solver passed each of the tests
    Description
      Text
        Use this function to run some basic checks with an SDP solver
      Example
        checkOptimize ("CSDP")
    Caveat
    SeeAlso
      Solver
///

doc ///
    Key
      "mosekexec"
      "csdpexec"
      "sdpaexec"
    Headline
      Locations of executables
    Description
      Text
        The package uses these mutable variables to store the paths of the executables.
	They can be edited by the user. The preferred way to do this is @TO changeSolver@, though.
///
