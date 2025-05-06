newPackage(
    "IntegerProgramming",
    Version => "0.1",
    Date => "April 21, 2025",
    Headline => "solving integer programs with Gröbner bases",
    Authors => {{
        Name => "Mike Cummings", 
        Email => "mike.cummings@uwaterloo.ca",
        HomePage => "https://mikecummings.ca"
    }},
    Keywords => {"Applied Algebraic Geometry"}
)

export {
    -- methods
    "adaptedMonomialOrder",
    "isFeasiblePoint",
    "solveILP",
    "toStandardForm",

    -- options
    "Field",
    "IsInStandardForm"
}

-* code section *-


-- [CLO, Chapter 8, Section 1, Exercise 7]
adaptedMonomialOrder = method(
    Options => {
        Field => QQ,
        Variables => {"w", "z"}
    }
)

adaptedMonomialOrder(Matrix, Matrix) := Ring => opts -> (A, c) -> (
    if numRows c > 1 and numColumns c > 1 then error("expected a one-dimensional matrix as the second argument");
    adaptedMonomialOrder(A, flatten entries c)
)

adaptedMonomialOrder(Matrix, List) := Ring => opts -> (A, c) -> (
    if min flatten entries A < 0 then error("expected nonnegative matrix entries");
    if any(join(flatten entries A, c), i -> not instance(i, ZZ)) then error("expected integer entries");

    numVars := numColumns A;
    numConstraints := numRows A;
    -- if numVars != #c then error("incompatible input sizes");

    w := getSymbol first opts.Variables;
    z := getSymbol last opts.Variables;

    R := (opts.Field)(monoid[z_1..z_numConstraints]);

    if min c >= 0 then (
        S := (opts.Field)(monoid[w_1..w_numVars, MonomialOrder=>{Weights=>c}]);
    ) else (
        degs := apply(numVars, j -> sum flatten entries A_{j});

        negativeIndices := select(#c, i -> c_i < 0);
        negativeCs := c_negativeIndices;
        negativeDs := degs_negativeIndices;  -- these aren't negative, but they correspond to the negative coefficients in the objective function

        mu := 1 - min apply(#negativeCs, i -> floor(negativeCs_i / negativeDs_i));  -- = 1 + max( ceiling(-c_i / d_i) )
        varWeights := apply(#c, i -> c_i + mu*degs_i);

        S = (opts.Field)(monoid[w_1..w_numVars, Degrees=>degs, MonomialOrder=>{Weights=>varWeights}]);
    );
    R ** S
)

-- [CLO, Chapter 8, Proposition 1.6]
isFeasiblePoint = method(
    Options => {
        Field => QQ
    }
)

isFeasiblePoint(Matrix, Matrix, Matrix) := Boolean => opts -> (A, b, X) -> (
    if numRows b > 1 and numColumns b > 1 then error("expected a one-dimensional matrix as the second argument");
    if numRows X > 1 and numColumns X > 1 then error("expected a one-dimensional matrix as the third argument");
    isFeasiblePoint(A, flatten entries b, flatten entries X, Field=>opts.Field)
)

isFeasiblePoint(Matrix, Matrix, List) := Boolean => opts -> (A, b, X) -> (
    if numRows b > 1 and numColumns b > 1 then error("expected a one-dimensional matrix as the second argument");
    isFeasiblePoint(A, flatten entries b, X, Field=>opts.Field)
)

isFeasiblePoint(Matrix, List, Matrix) := Boolean => opts -> (A, b, X) -> (
    if numRows X > 1 and numColumns X > 1 then error("expected a one-dimensional matrix as the third argument");
    isFeasiblePoint(A, b, flatten entries X, Field=>opts.Field)
)

isFeasiblePoint(Matrix, List, List) := Boolean => opts -> (A, b, X) -> (
    -- whether AX = b, where A and b have nonnegative entries 

    allEntries := join(flatten entries A, b, X);
    if any(allEntries, i -> not instance(i, ZZ)) then error("expected integer entries");
    if min allEntries < 0 then error("expected nonnegative entries");

    numVars := numColumns A;
    numConstraints := numRows A;
    -- if numVars != #X or numConstraints != #b then error("incompatible input sizes");
    if numConstraints != #b then error("incompatible input sizes");

    R := (opts.Field)(monoid[(getSymbol "w")_1..(getSymbol "w")_numVars]);
    S := (opts.Field)(monoid[(getSymbol "z")_1..(getSymbol "z")_numConstraints]);

    -- mapOutputs := matrix{for j from 1 to numVars list product apply(toList(1..numConstraints), i -> (S_(i-1))^(A_(i-1, j-1)))};
    mapOutputs := matrix{for j from 1 to numVars list product apply(numConstraints, i -> (S_i)^(A_(i, j-1)))};   -- check this (I changed this and didn't check it) ------------------------------
    phi := map(S, R, mapOutputs);

    phi(R_X) == S_b
)


-- [CLO, Chapter 8, Theorem 1.11], and the algorithm that follows
solveILP = method(
    Options => {
        Field => QQ,
        IsInStandardForm => true
    }
)

solveILP(Matrix, Matrix, Matrix) := opts -> (A, b, c) -> (
    if numRows b > 1 and numColumns b > 1 then error("expected a one-dimensional matrix as the second argument");
    if numRows c > 1 and numColumns c > 1 then error("expected a one-dimensional matrix as the third argument");
    transpose matrix {solveILP(A, flatten entries b, flatten entries c, Field=>opts.Field, IsInStandardForm=>opts.IsInStandardForm)}
)

solveILP(Matrix, Matrix, List) := opts -> (A, b, c) -> (
    if numRows b > 1 and numColumns b > 1 then error("expected a one-dimensional matrix as the second argument");
    transpose matrix {solveILP(A, flatten entries b, c, Field=>opts.Field, IsInStandardForm=>opts.IsInStandardForm)}
)

solveILP(Matrix, List, Matrix) := opts -> (A, b, c) -> (
    if numRows c > 1 and numColumns c > 1 then error("expected a one-dimensional matrix as the third argument");
    solveILP(A, b, flatten entries c, Field=>opts.Field, IsInStandardForm=>opts.IsInStandardForm)
)

solveILP(Matrix, List, List) := opts -> (A, b, c) -> (
    -- gives a solution that minimizes cx such that Ax = b is in standard form with all entries in A, b positive

    if not opts.IsInStandardForm then (
        (A', c') := toStandardForm(A, c);
        return (solveILP(A', b, c'))_{0..(#c-1)};
    );

    numVars := #c;
    numConstraints := numRows A;

    if any(join(flatten entries A, b, c), i -> not instance(i, ZZ)) then error("expected integer entries");
    if min join(flatten entries A, b) < 0 then error("expected nonnegative entries");
    -- if numVars != #c or numConstraints != #b then error("incompatible input sizes");
    if numConstraints != #b then error("incompatible input sizes");

    R := adaptedMonomialOrder(A, c);
    f := R_b;
    fs := apply(numVars, j -> R_(flatten entries A_{j}));
    I := ideal apply(numVars, j -> fs_j - R_(numConstraints + j));  -- this should be in R
    g := f % I;  -- this is a monomial by [CLO, Chapter 8, Proposition 1.8]


    if isSubset(support g, (gens R)_{numConstraints..(numConstraints+numVars-1)}) then (
        solution := first exponents g;  -- includes the coordinates corresponding to variables we eliminated
        return solution_{numConstraints..(numVars + numConstraints - 1)};
    ) else return false;
)


toStandardForm = method()
toStandardForm(Matrix) := Matrix => A -> (
    -- input: matrix A corresponding to constraints Ax <= b
    -- output: matrix A' corresponding to constraints A'x' = b
    n := numRows A;
    R := ring A;
    A | id_(R^n)
)

toStandardForm(Matrix, Matrix) := Sequence => (A, c) -> (
    -- input: matrix A corresponding to constraints Ax <= b and matrix c corresponding to objective function cx
    -- output: matrix A' and matrix c' corresponding to constraints A'x' = b and objective function c'x'
    
    if numRows c > 1 and numColumns c > 1 then error("expected a one-dimensional matrix as the second argument");
    
    output := toStandardForm(A, flatten entries c);
    if numRows c == 1 then (first output, matrix {last output}) else (first output, transpose matrix {last output})
)

toStandardForm(Matrix, List) := Sequence => (A, c) -> (
    -- input: matrix A corresponding to constraints Ax <= b and list c corresponding to objective function cx
    -- output: matrix A' and list c' corresponding to constraints A'x' = b and objective function c'x'
    (toStandardForm A, join(c, (numRows A):0))
)


-* unexported methods *-


-* Documentation section *-
beginDocumentation()

doc ///
    Key
        IntegerProgramming
    
    Headline
        solving integer programs with Gröbner bases
    
    Description
        Text
            Integer linear programming concerns optimization problems of the form 
            $$
            \begin{matrix}
                \text{minimize} & c^T x \\ 
                \text{such that} & Ax = b \\ 
                \text{and} & x \in \mathbb Z^n_{\ge 0}
            \end{matrix}
            $$
            for some $m \times n$ matrix $A$ and vectors $b$ and $c$.
            In general, solving integer linear programs is an NP-hard problem.
            Work from the early 1990s (see [CT, O, P]) developed algorithms for solving integer linear programs 
            using Gröbner bases.
            
            The key to this approach is constructing a term order that is compatible with the objective function 
            of the program.
            We implement this in @TO adaptedMonomialOrder@, which returns a ring equipped with such a term order.
            This order is used in @TO solveILP@ to solve integer programs.

    Acknowledgement
        This package was developed as part of the graduate course Computational Commutative Algebra and Algebraic Geometry during 
        the Fields Institute's Thematic Program in Commutative Algebra and Applications in Winter 2025.
        We thank Mike Stillman, for his instruction of the course, and the organizers of the thematic program: David Eisenbud, 
        Elisa Gorla, Megumi Harada, Jenna Rajchgot, Hal Schenck, and Adam Van Tuyl.

        Cummings is partially supported by NSERC CGS-D 588999--2024 and a University of Waterloo President's Graduate Scholarship.
    
    References
        [CT] Pasqualina Conti and Carlo Traverso. Buchberger algorithm and integer programming. 
                {\em Applied algebra, algebraic algorithms and error-correcting codes (New Orleans, LA, 1991)}, 130--139, 
                Lecture Notes in Comput. Sci., 539, Springer, Berlin, 1991.

        [CLO] David A. Cox, John Little, and Donal O'Shea. Using Algebraic Geometry. Second edition. Graduate Texts in Mathematics, 185. 
                {\em Springer, New York}, 2005.

        [O] François Ollivier . Canonical bases: relations with standard bases, finiteness conditions and application to tame automorphisms. 
            {\em Effective methods in algebraic geometry (Castiglioncello, 1990)}, 379–400, Progr. Math., 94, Birkhäuser Boston, Boston, MA, 1991.

        [P] Loïc Pottier . Minimal solutions of linear Diophantine systems: bounds and algorithms. 
            {\em Rewriting techniques and applications (Como, 1991)}, 162–173, Lecture Notes in Comput. Sci., 488, Springer , Berlin, 1991.

    Subnodes
        adaptedMonomialOrder
        isFeasiblePoint
        solveILP
        toStandardForm
        Field
        IsInStandardForm
///



doc ///
    Key
        adaptedMonomialOrder
        (adaptedMonomialOrder, Matrix, List)
        (adaptedMonomialOrder, Matrix, Matrix)
        [adaptedMonomialOrder, Variables]
    
    Headline
        constructs a ring with monomial order adapted to an integer program
    
    Usage
        adaptedMonomialOrder(A, c)
        adaptedMonomialOrder(A, c, Variables=>L)
    
    Inputs
        A: Matrix
            with nonnegative entries in @TO ZZ@
        c: {Matrix, List}
            with entries in @TO ZZ@
        L: List
            optionally, a list of length two containing strings that we use as variable names in the ring
    
    Outputs
        :Ring
            equipped with a term order adapted to the given integer program
    
    Description
        Text
            Solving integer linear programs using Gröbner bases relies on constructing a monomial order that is {\em adapted} to the program at hand.
            This method returns a ring equipped with such an order.

        Text
            {\bf Definition.}
            Fix a matrix $A \in \mathbb Z^{m \times n}$ and vectors $b \in \mathbb Z^m$, $c \in \mathbb Z^n$.
            Consider the integer linear program in @TO2 {toStandardForm, "standard form"}@: 
            $$\text{minimize } c^Tx \quad \text{subject to } Ax = b \text{ and } x \in \mathbb Z_{\ge 0}^n.$$
            Let $\varphi$ be as in @TO isFeasiblePoint@.
            A monomial order $<$ on $k[z_1, \ldots, z_m, w_1, \ldots, w_n]$ is {\em adapted} to the program if both of the following hold:

            1. (Elimination) $z_i > w^\alpha$ for all $\alpha \in \mathbb Z_{\ge 0}^n$, for all $i = 1, \ldots, m$;

            2. (Compatibility with the program) if $c^T \alpha > c^T \beta$ and $\varphi(w^\alpha) = \varphi(w^\beta)$, then $w^\alpha > w^\beta$.

        Text
            {\bf Construction.}
            We outline here the construction of such an order, following Exercise 7 in [CLO, Chapter 8, Section 1].

            First, suppose that the vector $c$ defining the objective function contains only nonnegative entries.
            Define a weight order on the variables $w_1, \ldots, w_m$ by giving weight $c_j$ to $w_j$.
            We extend this to an adapted monomial order on $k[z_1, \ldots, z_n, w_1, \ldots, w_m]$ by treating each $z_i$ as lexicographically larger than every $w_j$.

            Assume now that $c$ is not nonnegative.
            For each $j = 1, \ldots, n$, define $d_j = \sum_{i=1}^m A_{i,j}$.
            (Note that $d_j > 0$ for otherwise the constraints in the linear program do not depend on $x_j$.)
            Equip $k[z_1, \ldots, z_m, w_1, \ldots, w_m]$ with nonstandard grading: $\deg(z_i) = 1$ for all $i$ and $\deg(w_j) = d_j$ for all $j$.
            Now for the term order, pick $\mu$ such that the vector $(c_1, \ldots, c_n) + \mu(d_1, \ldots, d_n)$ contains only nonnnegative entries.
            This gives rise to a weight order on $k[w_1, \ldots, w_n]$ which satisfies condition (2) above.
            We extend this to an adapted monomial order on $k[z_1, \ldots, z_n, w_1, \ldots, w_m]$ by again treating each $z_i$ as lexicographically larger than every $w_j$.
    
    References
        [CLO] David A. Cox, John Little, and Donal O'Shea. Using Algebraic Geometry. Second edition. Graduate Texts in Mathematics, 185. Springer, New York, 2005.
    
    SeeAlso
        solveILP
        toStandardForm
        Field

///

doc ///
    Key
        isFeasiblePoint
        (isFeasiblePoint, Matrix, List, List)
        (isFeasiblePoint, Matrix, List, Matrix)
        (isFeasiblePoint, Matrix, Matrix, List)
        (isFeasiblePoint, Matrix, Matrix, Matrix)
    
    Headline
        whether a point is feasible to an integer linear program
    
    Usage
        isFeasiblePoint(A, b, X)
    
    Inputs
        A: Matrix
            with nonnegative entries in @TO ZZ@
        b: {Matrix, List}
            with nonnegative entries in @TO ZZ@
        X: {Matrix, List}
            with nonnegative entries in @TO ZZ@
    
    Outputs
        :Boolean
    
    Description
        Text            
            Fix a matrix $A \in \mathbb Z^{m \times n}$ and integer linear program in @TO2 {toStandardForm, "standard form"}@ with constraints $Ax = b$.
            The following is a characterization of when the vector $x \in \mathbb Z_{\ge 0}^n$ satisfies $Ax = b$, in which case we say that it is {\em feasible}.
            This condition is exactly [CLO, Chapter 8, Proposition 1.6].

        Text
            {\bf Proposition.} 
            Let $A \in \mathbb Z^{m \times n}$ and consider the feasible region $Ax = b$.
            Define a homomorphism of rings $\varphi:k[w_1, \ldots, w_n] \to k[z_1, \ldots, z_m]$, where $k$ is any @TO2 {Field, "field"}@, by 
            $$\varphi: w_j \mapsto \prod_{i=1}^m z_i^{A_{i, j}} \quad \text{for all } j = 1, \ldots, n.$$
            Then, a vector $x \in \mathbb Z_{\ge 0}^n$ satisfies $Ax = b$ if and only if $\varphi(w^x) = z^b$.

        Example
            A = matrix{{1, 2}, {3, 4}}
            b = matrix{{4}, {10}}
            isFeasiblePoint(A, b, {1, 1})
            isFeasiblePoint(A, b, matrix{{2}, {1}})
    References
        [CLO] David A. Cox, John Little, and Donal O'Shea. Using Algebraic Geometry. Second edition. Graduate Texts in Mathematics, 185. Springer, New York, 2005.

    Caveat
        Assumes the program is @TO2 {toStandardForm, "in standard form"}@.
    
    SeeAlso
        solveILP
        Field
        IsInStandardForm
///

doc ///
    Key
        solveILP
        (solveILP, Matrix, List, List)
        (solveILP, Matrix, List, Matrix)
        (solveILP, Matrix, Matrix, List)
        (solveILP, Matrix, Matrix, Matrix)
    
    Headline
        solve an integer linear program in standard form
    
    Usage
        solveILP(A, b, c)
    
    Inputs
        A: Matrix
            with nonnegative entries in @TO ZZ@
        b: {Matrix, List}
            with nonnegative entries in @TO ZZ@
        c: {Matrix, List}
            with entrines in @TO ZZ@
    
    Outputs
        :Boolean 
            {\tt false} if there is no solution, otherwise:
        :{Matrix, List}
            a minimizer of the ILP, if a solution exists, of the same type as {\tt b}
    
    Description
        Text
            Consider the integer linear program
            $$\text{minimize } c^T x \quad\text{subject to } Ax = b \text{ and } x \in \mathbb Z_{\ge 0}^n,$$
            where $A$ is an $m \times n$ integer-valued matrix and $b \in \mathbb Z^m$, $c \in \mathbb Z^n$.
            Suppose that every entry in $A$ and $b$ is nonnegative.
            A solution to the problem is given by [CLO, Chapter 8, Theorem 1.11], which we now detail.

            We work in the polynomial ring $R = k[z_1, \ldots, z_m, w_1, \ldots, w_n]$, where $k$ is a @TO2 {Field, "field"}@, 
            equipped with an @TO2 {adaptedMonomialOrder, "monomial order adapted to the program"}@.
            For each $j = 1, \ldots, n$, let $f_j = \prod_{i=1}^m z_i^{A_{i,j}}$ and let $\mathcal G$ be a Gröbner basis for 
            $\langle f_1 - w_1 , \ldots, f_n - w_n \rangle$ in $R$.
            Consider the remainder on division $\overline f^{\mathcal G}$ of $f = z^b$.
            The result of [CLO, Chapter 8, Proposition 1.8] guarantees that $\overline f^{\mathcal G}$ is a monomial.
            Moreover, the program is feasible if $\overline f^{\mathcal G}$ involves only the variables $w_1, \ldots, w_n$ and, 
            in this case, a solution is given by the exponent vector of $\overline f^{\mathcal G}$.

            The following example is from [CLO, Chapter 8, Section 1].

        Example
            A = matrix{{4, 5, 1, 0}, {2, 3, 0, 1}}
            b = {37, 20}
            c = {-11, -15, 0, 0}
            solveILP(A, b, c)
        Text
            Alternately, one can write the program not in standard form, see @TO IsInStandardForm@.
            
        Text
            {\bf Remark.}
            Maximization and minimization (integer) linear programs are equivalent by multiplying objective functions by $-1$.
            This package treats every problem as a minimization problem and leaves the multiplication for maximiation problems to the user.

    References
        [CLO] David A. Cox, John Little, and Donal O'Shea. Using Algebraic Geometry. Second edition. Graduate Texts in Mathematics, 185. Springer, New York, 2005.

    Caveat
        This method produces only one solution (that is, a feasible point that attains the minimum), even when multiple solutions exist.
        
    SeeAlso
        adaptedMonomialOrder
        isFeasiblePoint
        toStandardForm
        Field
        IsInStandardForm
///

doc ///
    Key
        toStandardForm
        (toStandardForm, Matrix)
        (toStandardForm, Matrix, List)
        (toStandardForm, Matrix, Matrix)
    
    Headline
        converts an LP to standard form
    
    Usage
        toStandardForm A
        toStandardForm(A, c)
    
    Inputs
        A: Matrix
        c: {Matrix, List}
    
    Outputs
        : {Matrix, Sequence}
            the standard form corresponding to the given program 

    Description
        Text
            The {\bf standard form} of an integer linear program is 
            $$\text{minimize } c^T x \quad\text{subject to } Ax = b \text{ and } x \in \mathbb Z_{\ge 0}^n.$$
            An integer linear program with constraints $Bx \le b$ is equivalent to one in standard form in the following way:
            One constraint $B_{j,1}x_1 + \cdots + B_{j,r}x_r \le d_j$ in this system is equivalent to the pair of constraints 
            $B_{j,1}x_1 + \cdots + B_{j, r}x_r + y_j = d_j$ and $y_j \ge 0$.

            Hence we rewrite the constrains $Bx \le b$ as $Ax = b$ by introducing {\em slack variables} $y_j$ and @TO2{"|", "concatenating"}@ a copy of the identity matrix
            {\tt A = B|I} of the appropriate size.
            (Note that we abuse notation here and write $x$ for both the vector of the given program and the vector that includes the slack variables $y_j$.)

            This equivalence does not change the objective function; we pad $c$ with a zero for each slack variable introduced.

        Example
            A = matrix{{1, 2}, {3, 4}}
            c = matrix{{5}, {6}}
            toStandardForm(A, c) 
    SeeAlso
        adaptedMonomialOrder
        solveILP
///

doc ///
    Key
        Field
        [adaptedMonomialOrder, Field]
        [isFeasiblePoint, Field]
        [solveILP, Field]
    
    Headline
        field over which to define polynomial rings

    Description
        Text 
            The field used for computations in @TO adaptedMonomialOrder@, @TO isFeasiblePoint@, @TO solveILP@, default value @TO QQ@.

    SeeAlso
        adaptedMonomialOrder
        isFeasiblePoint
        solveILP
///

doc ///
    Key
        IsInStandardForm
        [solveILP, IsInStandardForm]

    Headline
        specify whether the integer program is in standard form

    Description
        Text
            An optional method for @TO solveILP@ to specify whether a program is in @TO2 {toStandardForm, "standard form"}@.
            The default value is {\tt true}.
            If {\tt IsInStandardForm=>false}, we first convert @TO2 {toStandardForm, "to standard form"}@ before solving.

            The following example is in [CLO, Chapter 8, Section 1].
            Note that in contrast to @TO solveILP@ with {\tt IsInStandardForm=>true}, the output involves only variables that 
            are in the specified program, meaning that the slack variables, that get introduced to convert to standard form, 
            are omitted in the following output.

        Example
            A = matrix{{4, 5}, {2, 3}}
            b = matrix{{37}, {20}}
            c = matrix{{-11, -15}}
            solveILP(A, b, c, IsInStandardForm=>false) 
    SeeAlso
        solveILP
        toStandardForm
///


-* Test section *-
TEST /// -* isFeasiblePoint [CLO, Chapter 8, Section 1] *-
A = matrix{
    {4, 5, 1, 0}, 
    {2, 3, 0, 1}
};
b = {37, 20};
assert(isFeasiblePoint(A, b, {4, 4, 1, 0}))
///

TEST /// -* solveILP [CLO, Chapter 8, Section 1] *-
A = matrix{
    {4, 5, 1, 0}, 
    {2, 3, 0, 1}
};
b = {37, 20};
c = {-11, -15, 0, 0};
assert(solveILP(A, b, c) == {4, 4, 1, 0})
/// 

TEST /// -* [solveILP, IsInStandardForm=>false] [CLO, Chapter 8, Section 1] *-
A = matrix{
    {4, 5},
    {2, 3}
};
b = {37, 20};
c = {-11, -15};
assert(solveILP(A, b, c, IsInStandardForm=>false) == {4, 4})
///

TEST ///  -* toStandardForm [CLO, Chapter 8, Section 1] *-
A = matrix{
    {4, 5},
    {2, 3}
};
c = {-11, -15};
assert(toStandardForm(A, c) == (matrix{{4, 5, 1, 0}, {2, 3, 0, 1}}, {-11, -15, 0, 0}))
///


end--


-* Development section *-
restart
debug needsPackage "IntegerProgramming"
check "IntegerProgramming"

uninstallPackage "IntegerProgramming"
restart
installPackage "IntegerProgramming"
viewHelp "IntegerProgramming"


