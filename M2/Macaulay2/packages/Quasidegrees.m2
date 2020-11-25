newPackage(
	"Quasidegrees",
    	Version => "1.0", 
    	Date => "June 24, 2015",
    	Authors => {{Name => "Roberto Barrera", 
		  Email => "rbarrera@math.tamu.edu", 
		  HomePage => "http://www.math.tamu.edu/~rbarrera/"}},
    	Headline => "quasidegrees and graded local cohomology",
	Keywords => {"Commutative Algebra"},
	PackageImports => {"FourTiTwo", "Depth", "Polyhedra"},
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry",
	     "journal URI" => "http://j-sag.org/",
	     "article title" => "Computing quasidegrees of A-graded modules",
	     "acceptance date" => "26 February 2019",
	     "published article URI" => "https://msp.org/jsag/2019/9-1/p04.xhtml",
	     "published article DOI" => "10.2140/jsag.2019.9.29",
	     "published code URI" => "https://msp.org/jsag/2019/9-1/jsag-v9-n1-x04-Quasidegrees.m2",
	     "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/Quasidegrees.m2",
	     "release at publication" => "d76252d2c8d38f0ec55212eb458869503b1f0312",	    -- git commit number in hex
	     "version at publication" => "1.0",
	     "volume number" => "9",
	     "volume URI" => "https://msp.org/jsag/2019/9-1/"
	     }
    	)

export{"makeGradedRing",
	"toGradedRing",
	"toricIdeal",
	"quasidegreesAsVariables",
	"quasidegrees",
	"quasidegreesLocalCohomology",
	"exceptionalSet",
	"removeRedundancy",
	"qav" => "quasidegreesAsVariables",
	"qlc" => "quasidegreesLocalCohomology"
	}


--==================================--
--        Unexported Methods        --
--==================================--

------------------------------------------------
-- isMonomial

-- Checks if a polynomial is a monomial or zero.
------------------------------------------------

isMonomial = method()
isMonomial(RingElement) := (f) -> (
    #terms(f) <= 1
    )

isMonomial(ZZ) := (n) -> (
    true
    )


------------------------------------------------
-- isMonomialMatrix

-- Checks if every entry in a matrix is a monomial or zero.
------------------------------------------------

isMonomialMatrix = method()
isMonomialMatrix(Matrix) := (M) -> (
    all(flatten entries M, m -> isMonomial(m))
    )


------------------------------------------------
-- isPositivelyGraded

-- Checks if a polynomial ring or the ring of a module over
-- a polynomial ring is positively graded.
------------------------------------------------

isPositivelyGraded = method()
isPositivelyGraded(Ring) := (R) -> (
    isPointed posHull(transpose matrix degrees R)
    )
    
isPositivelyGraded(Module) := (M) -> (
    R := ring M;
    isPositivelyGraded R
    )


--================================--
--        Exported Methods        --
--================================--

------------------------------------------------
-- makeGradedRing

-- input: A, dxn integer matrix
--        x, symbol
-- output: graded polynomial  ring QQ[x_0..x_(n-1)] 
--         with the deg(x_i) = (i+1)th column of A.
------------------------------------------------

makeGradedRing = method()
makeGradedRing(Matrix,Symbol) := (A,x) -> (
    degs := entries(transpose(A));
    QQ[x_0..x_(numgens(source(A))-1),Degrees=>degs]
    )


------------------------------------------------
-- toGradedRing

-- input: A, dxn integer matrix
--        R, polynomial ring in n variables
-- output: R, polynomial ring with variables graded 
--            by columns of A.
------------------------------------------------

toGradedRing = method()
toGradedRing(Matrix,PolynomialRing) := (A,R) -> (
    if numgens R =!= rank source A then error "expected numgens R to equal number of columns of A";
    degs := entries transpose A;
    k := coefficientRing R;
    v := toSequence flatten entries vars R;
    k[v,Degrees=>degs]
    )


------------------------------------------------
-- toricIdeal

-- input: A, a dxn integer matrix
--        R, polynomial ring
-- output: I, toric ideal in R associated to A.

-- The toric ideal is computed by saturating the lattice basis
-- ideal of the kernel of A with respect to the product of the 
-- variables of the polynomial ring.
------------------------------------------------

toricIdeal = method()
toricIdeal(Matrix,Ring) := (A,R) -> (
    m := product gens R;
    saturate(toBinomial(transpose(syz(A)),R),m)	
    )


------------------------------------------------
-- quasidegreesAsVariables

-- input: M, finitely generated module over a polynomial ring.
-- output: Q, a list that represents the quasidegree set of M
--            by the variables of the polynomial ring.

-- The output is a list of pairs {x^u,F}
-- where x^u is a monomial and F is a subset of variables of 
-- the polynomial ring.  F is meant to represent a plane in 
-- the variables of F and x^u is a shift of the plane by u.

-- For example, a pair {x^u,{x_1,x_2,x_4}} corresponds to a
-- x_1x_2x_4-plane shifted in the direction of u.

-- When the input is an ideal I instead of a module, 
-- quasidegreesAsVariables is executed on the module R^1/I where
-- R is the ring of I. 
------------------------------------------------

quasidegreesAsVariables = method()
quasidegreesAsVariables(Module) := (M) -> (
    R := ring M;
    P := presentation M;
    if not isMonomialMatrix P then error "expected module to be presented by a matrix with monomial entries";
    if not isPositivelyGraded M then error "module is not positively graded";
    if not isHomogeneous M then error "module is not homogeneous with respect to ambient grading";
    S := entries P;
    stdPairs := {};
    for s to (#S-1) do stdPairs = stdPairs|standardPairs(monomialIdeal(S#s));
    stdPairs
    )


quasidegreesAsVariables(Ideal) := (I) -> (
    R := ring I;
    M := R^1/I;
    quasidegreesAsVariables M
    )


------------------------------------------------
-- quasidegrees

-- input: M, a finitely generated module over a polynomial ring.
-- output: Q, a list that represents the quasidegree set of M.

-- The output is a list of pairs (u,F) where u is a vector in ZZ^d and
-- F is a list of vectors in ZZ^d.
-- Each pair (u,F) represents the complex plane u+span(F).
------------------------------------------------

quasidegrees=method()
quasidegrees(Module) := (M) -> (
    R := ring M;
    P := presentation M;
    if not isMonomialMatrix P then error "expected module to be presented by a matrix with monomial entries";
    if not isPositivelyGraded M then error "module is not positively graded";
    if not isHomogeneous M then error "module is not homogeneous with respect to ambient grading";
    E := entries P;
    D := degrees target P;
--  We make the following list of pairs S.  
--  The first entry is a degree twist in the presentation of M.
--  The second entry are the standard pairs of the corresponding row.
    S := apply(#D, i -> (
	    {vector(D_i), standardPairs monomialIdeal E_i}
	    )
	);
--  Next we make a list T representing the quasidegree set of M as follows.
--  The variables in S get assigned their degrees and then
--  shifted by the corresponding degree twist in the
--  presentation of M.
    T := flatten(apply(S, s-> (
		apply((s_1), w -> (
			if w_0==1 then (
			    if w_1 =={} then (
				{s_0,{}}
				)
			    else {s_0, apply(w_1, x -> vector degree x)}
			    )
			else{s_0 + (vector(degree(w_0))), apply(w_1, x -> vector degree x)}
			)
		    )
		)
	    )
	);
    toList set T
    )


quasidegrees(Ideal) := (I) -> (
    R := ring I;
    M := R^1/I;
    quasidegrees M
    )


------------------------------------------------
-- quasidegreesLocalCohomology

-- input: i, an integer
--        M, a module
-- output: Q, a list representing the quasidegree set
--            of a local cohomology module of M.

-- Computes the quasidegree set of the i-th local cohomology module
-- at the maximal ideal for a finitely generated module over a
-- polynomial ring.

-- We use Local Duality to compute the quasidegree set of the local 
-- cohomology modules.  We compute the quasidegree set of Ext^(n-i)(M,R)
-- and then a degree shift is applied.  The code is the
-- same as quasidegrees with additional shifts coming from Local
-- Duality

-- The output is a list of pairs (u,F) where u is a vector in ZZ^d and
-- F is a list of vectors in ZZ^d.
-- Each pair (u,F) represents the complex plane u+span(F).

-- When the input has an ideal I instead of a module, quasidegreesLocalCohomology 
-- is executed with the module R^1/I where R is the ring of I.
------------------------------------------------

quasidegreesLocalCohomology = method()
quasidegreesLocalCohomology(ZZ, Module) := (i,M) -> (
    R := ring M;
    n := numgens R;
    v := gens R;
    e := vector sum apply(v,x -> degree x);
--  use Local Duality
    N := Ext^(n-i)(M,R);
    P := presentation N;
    if not isMonomialMatrix P then error "expected module to be presented by a matrix with monomial entries";
    if not isPositivelyGraded N then error "module is not positively graded";
    if not isHomogeneous N then error "module is not homogeneous with respect to ambient grading";
    E := entries P;
    D := degrees target P;
--  We make the following list of pairs S.  
--  The first entry is a degree twist in the presentation of M.
--  The second entry is a list of the standard pairs of the monomial ideal
--  generated by the entries of the corresponding row.
    S := apply(#D, i -> (
	    {vector(-D_i), standardPairs monomialIdeal E_i}
	    )
	);
--  We next make a list T representing the quasidegree set of M.
--  The variables in S are assigned their degrees and then
--  shifted by the corresponding degree twist in the
--  presentation of M.
    T := flatten(apply(S, s-> (
		apply((s_1), w -> (
			if w_0==1 then (
			    if w_1 =={} then (
				{s_0,{}}
				)
			    else {s_0, apply(w_1, x -> vector degree x)}
			    )
			else{s_0 - (vector(degree(w_0))), apply(w_1, x -> vector degree x)}
			)
		    )
		)
	    )
	);
    Q := toList set T;
    apply( #Q, j -> {((Q_j)_0)-e,(Q_j)_1})
    )


quasidegreesLocalCohomology(ZZ, Ideal) := (i,I) -> (
    R := ring I;
    M := R^1/I;
    quasidegreesLocalCohomology(i,M)
    )


------------------------------------------------
-- quasidegreesLocalCohomology

-- input: M, a module over a polynomial ring
-- output: Q, a list representing the quasidegree set
--            of the non-top local cohomology modules of M.

-- Computes the quasidegrees of the i-th local cohomology module
-- at the maximal ideal of a module for 0<=i<d.
-- This method runs quasidegreesLocalCohomology(i,M) for 0<=i<d.

-- The output is a list of pairs (u,F) where u is a vector in ZZ^d and
-- F is a list of vectors in ZZ^d.
-- Each pair (u,F) represents the complex numbers u+span(F).

-- When the input is an ideal I instead of a module, the quasidegree
-- set of the local cohomology modules of R/I, where R is the ring of I,
-- is computed.
------------------------------------------------

quasidegreesLocalCohomology(Module) := (M) ->( 
    Q := for i from 0 to (dim M)-1 list quasidegreesLocalCohomology(i,M);
    Q' := delete({}, Q);
    flatten Q'
    )


quasidegreesLocalCohomology(Ideal) := (I) -> (
    R := ring I;
    M := R^1/I;
    quasidegreesLocalCohomology M   
    )


------------------------------------------------
-- exceptionalSet

-- input: A, a dxn integer matrix
-- output: E, a list, the exceptional parameters of A.

-- The method exceptionalSet takes a matrix A and computes the
-- quasidegree set of R/I where R is an A-graded polynomial ring and 
-- I is the toric ideal associated to A in R.
------------------------------------------------

exceptionalSet = method()
exceptionalSet(Matrix) := (A) -> (
    x := symbol x;
    R := makeGradedRing(A,x);
    I := toricIdeal(A,R);
    M := R^1/I;
    quasidegreesLocalCohomology M
    )


------------------------------------------------
-- removeRedundancy

-- Removes the redundancies of a quasidegree set.
------------------------------------------------

removeRedundancy = method()
removeRedundancy(List) := (L) -> (
    for i from 0 to #L-1 do(
	S := {};
	for j in i+1..#L-1 do(
	    if rank matrix(L_j)_1 == rank(matrix(L_j)_1|matrix(L_i)_0) then(
		for k from 0 to #((L_i)_1)-1 do(
		    if (rank matrix(L_j)_1 == rank((matrix(L_j)_1)|matrix((L_i)_1)_k)) then S=S|{1};
		    if S == toList(#((L_i)_1):1) then L = delete(L_i,L);
		    )
		)
	    )
	);
    L
    )



--================================================--
--**************** DOCUMENTATION *****************--
--================================================--

beginDocumentation()

doc ///
  Key
   Quasidegrees
  Headline
     a package to compute quasidegrees
  Description
   Text
    @EM "Quasidegrees"@ is a package that enables the user to construct multigraded rings
    and look at the graded structure of multigraded finitely generated modules over a
    polynomial ring.  The quasidegree set of a $\ZZ^d$-graded module $M$ is the Zariski closure 
    in ${\mathbb C}^d$ of the degrees of the nonzero homogeneous components of $M$.  This package 
    can compute the quasidegree set of a finitely generated module over a $\ZZ^d$-graded 
    polynomial ring.  This package also computes the quasidegree sets of local cohomology modules 
    supported at the maximal irrelevant ideal of modules over a $\ZZ^d$-graded polynomial ring.
    
    The motivation for this package comes from $A$-hypergeometric functions and the relation
    between the rank jumps of $A$-hypergeometric systems and the quasidegree sets of 
    non-top local cohomology modules supported at the maximal irrelevant ideal of the associated
    toric ideal as described in the paper:  
    
    Laura Felicia Matusevich, Ezra Miller, and Uli Walther. {\it Homological methods for 
    hypergeometric families}. J. Am. Math. Soc., 18(4):919-941, 2005.
    
    This package requires @TO FourTiTwo@, @TO Depth@, and @TO Polyhedra@.
  
  Caveat
    This package is written when the ambient ring of the modules in question are positively
    graded and are presented by a monomial matrix, that is, a matrix whose entries are 
    monomials.  This is due to the algorithms depending on finding standard pairs of 
    monomial ideals generated by rows of a presentation matrix. 
///

-----------------------------------
-- Documentation makeGradedRing --
-----------------------------------

doc ///
  Key
   makeGradedRing
   (makeGradedRing,Matrix,Symbol)
  Headline
   makes a polynomial ring graded by a matrix
  Usage
   makeGradedRing A
  Inputs 
   A: 
    a d x n integer matrix
   x:
    the variable of the multigraded polynomial ring
  Outputs
   :PolynomialRing
    a multigraded polynomial ring in n variables
  Description
   Text
    This method takes a $d\times n$ integer matrix $A$ and makes the polynomial ring
    $\QQ[x_0,..,x_{n-1}]$ with the degree of the i-th variable being the i-th column
    of $A$.
  
   Example
    A = matrix{{1,1,1,1,1},{0,0,1,1,0},{0,1,1,0,-2}}
    R = makeGradedRing(A,t)
  
   Text
    We can see that $R$ is graded by the columns of $A$
  
   Example
    describe R
    
///

---------------------------------
-- Documentation toGradedRing --
---------------------------------

doc ///
    Key
     toGradedRing
     (toGradedRing,Matrix,PolynomialRing)
    Headline
     grade a polynomial ring by a matrix
    Usage
     toGradedRing(A,R)
    Inputs
     A: Matrix
      a d x n integer matrix
     R: PolynomialRing
      a polynomial ring in n variables
    Outputs
     :PolynomialRing
      a polynomial ring graded by the columns of A
    Description
     Text
      This method takes a polynomial ring $R$ in $n$ variables and a $d\times n$ matrix $A$ and 
      grades $R$ by assigning the i-th variable of $R$ to have degree being the i-th 
      column of $A$.
     
     Example
      A=matrix{{1,1,1,1,1},{0,0,1,1,0},{0,1,1,0,-2}}
      R=QQ[a..e]
      S=toGradedRing(A,R)
      describe S

///

--------------------------------
--  Documentation toricIdeal  --
--------------------------------

doc ///
    Key
     toricIdeal
     (toricIdeal,Matrix,Ring)
    Headline
     returns a toric ideal
    Usage
     toricIdeal(A,R)
    Inputs
     A: Matrix
      a d x n integer matrix
     R: Ring
      a polynomial ring in n variables
    Outputs
     :Ideal
      the toric ideal associated to A in R
    Description
     Text
      Given a $d\times n$ @TO Matrix@ A and a polynomial ring in $n$ variables $R$, this method 
      returns the toric ideal associated to $A$ in $R$.  To do this, {\tt toricIdeal} saturates 
      the lattice basis ideal of the kernel of $A$ with respect to the product of the variables 
      of $R$.
     
     Example
	A=matrix{{1,1,1,1,1,1},{1,2,1,2,3,0},{0,2,2,0,1,1}}
        R=QQ[a..f]
	toricIdeal(A,R)
     
     Text
     
     Example
	A=matrix{{1,1,1,1,1},{0,0,1,1,0},{0,1,1,0,-2}}
	R=toGradedRing(A,QQ[a..e])
	toricIdeal(A,R)
///


-------------------------------------------
-- Documentation quasidegreesAsVariables --
-------------------------------------------

doc ///
    Key
     quasidegreesAsVariables
     (quasidegreesAsVariables,Ideal)
     (quasidegreesAsVariables,Module)
    Headline
     represents the quasidegree set in variables
    Usage
     quasidegreesAsVariables(I)
     quasidegreesAsVariables(M)
    Inputs
     I: Ideal
     M: Module
      a finitely generated module
    Outputs
     :List
      a list that indexes the quasidegrees of M as variables
    Description
     Text
      Given a finitely generated module over a $\ZZ^d$-graded polynomial ring $R$, 
      {\tt quasidegreesAsVariables} gives a representation of the quasidegree set of $M$ 
      using the variables of $R$. This method captures the plane arrangement of the 
      quasidegree set of the module.  
      
     Text
      If the input is an ideal $I$, then {\tt quasidegreesAsVariables} executes for the module
      $R/I$ where $R$ is the ring of $I$.
      
     Text
      A synonym for this function is @TT "qav"@.
     
     Example
      R = QQ[x,y,Degrees=>{{1,0},{0,1}}]
      I = ideal(x^2*y,x*y^2,y^3)
      M = R^1/I
      quasidegreesAsVariables M
      
     Text
      In the above example, the first element in the list {\tt \{1,\{x\}\}} corresponds to a 
      line in the $x$ direction with no shift.  The element {\tt \{y,\{\}\}} corresponds to a 
      point shifted in the direction of the degree of $y$, the element {\tt \{x*y,\{\}\}} 
      corresponds to a point shifted in the direction of the degree $xy$, and the element 
      {\tt \{y^2,\{\}\} }corresponds to a point shifted in the direction of the degree of 
      $y^2$.
      
      The next example has a 2 dimensional quasidegree set.
    
     Example
      R=QQ[x,y,z,Degrees=>{{1,0,0},{0,1,0},{0,0,1}}]
      I=ideal(y)
      M=R^1/I
      quasidegreesAsVariables M
     
     Text
      The quasidegree set of $\QQ[x,y,z]/<y>$ with the standard $\ZZ^3$-grading is the 
      (unshifted) $xz$-plane.
///

--------------------------------
-- Documentation quasidegrees --
--------------------------------

doc ///
    Key
     quasidegrees
     (quasidegrees,Ideal)
     (quasidegrees,Module)
    Headline
     compute the quasidegree set of a module
    Usage
     quasidegrees I
     quasidegrees M
    Inputs
     I: Ideal
     M: Module
      a finitely generated module
    Outputs
     :List
      a list that indexes the quasidegrees of M
    Description
     Text
      The method quasidegrees takes a finitely generated module $M$ over the polynomial ring  
      that is presented by a monomial matrix and computes the quasidegree set of $M$.  The 
      quasidegrees of $M$ are indexed by a list of pairs $(v,F)$ where $v$ is a vector and $F$ 
      is a list of vectors f@SUB TT"1"@,...,f@SUB TT"l"@.  The pair $(v,F)$ indexes the plane 
      $v+span_{\mathbb C}F$.  The quasidegree set of M is the union of all such planes that the 
      pairs (v,F) index.
     
     Text
      If the input is an ideal $I$, then {\tt quasidegrees} executes for the module $R/I$ where
      $R$ is the ring of $I$.
     
     Text
      The following example computes the quasidegree set of 
      $\QQ[x,y]/<x^2,y^2>$ under the standard $\ZZ^2$-grading.
     
     Example
      A = matrix{{1,0},{0,1}}
      R = QQ[x,y, Degrees => entries transpose A]
      I = ideal(x^2,y^2)
      M = R^1/I
      quasidegrees M
     
     Text
      The quasidegree set is given to be the points (0,1), (1,0), (1,1), and (0,0).
     
     Text
      The next example takes $R$ computes the quasidegrees of the above module after
      twisting $R$ by multidegree (3,2).
      
     Example
      R = R^{{-3,-2}}
      M = R^1/I
      quasidegrees M 
     
     Text
      The following demonstrates a quasidegree set that is not a finite number of points.
     
     Example
      A = matrix{{1,0},{0,1}}
      R = QQ[x,y]
      R = toGradedRing(A,R)
      I = ideal(x^2*y,y^2)
      M=R^1/I
      quasidegrees M
	
     Text
      In the above example, the quasidegree set of the module M consists of the points (1,1)
      and (0,1) along with the parameterized line (1,0)$\bullet t$.
///

-----------------------------------------------
-- Documentation quasidegreesLocalCohomology --
-----------------------------------------------

doc ///
    Key
     quasidegreesLocalCohomology
     (quasidegreesLocalCohomology,ZZ,Module)
     (quasidegreesLocalCohomology,ZZ,Ideal)
     (quasidegreesLocalCohomology,Module)
     (quasidegreesLocalCohomology,Ideal)
    Headline
     returns the quasidegree sets of local cohomology modules
    Usage
     quasidegreesLocalCohomology(I)
     quasidegreesLocalCohomology(M)
     quasidegreesLocalCohomology(i,I)
     quasidegreesLocalCohomology(i,M)
    Inputs
     i: ZZ
      the cohomological degree to be computed
     I: Ideal
      an ideal in a multigraded polynomial ring
     M: Module
      a module over a multigraded polynomial ring
    Outputs
     :List
      that represents the quasidegree set of local cohomology modules
    Description
     Text
      The input for this method is a  module $M$ over a multigraded polynomial ring whose local
      cohomology modules can be presented by monomial matrices.  If an integer $i$ is also 
      included in the input, {\tt quasidegreesLocalCohomology(i,M)} computes the quasidegree 
      set of the $i-th$ local cohomology module, supported at the maximal irrelevant ideal, of 
      $M$.  If an integer is excluded from the input, then {\tt quasidegreesLocalCohomology(M)} 
      computes the quasidegree set of $H_{\bf m}^0(M)\oplus\cdots\oplus H_{\bf m}^{d-1}(M)$. 
      The quasidegrees of local cohomology are indexed by a list of pairs $(v,F)$ where $v$ is 
      a vector and $F$ is a list of vectors.  The pair $(v,F)$ indexes the plane 
      $v+span_{\mathbb C}F$.  The quasidegree set of the local cohomology modules is the union 
      of all such planes that the pairs $(v,F)$ index.
     
     Text
      If the input is an ideal $I$ in a multigraded polynomial ring $R$, then the method 
      executes for the module $R/I$ where $R$ is the ring of $I$.
      
     Text
      A synonym for this function is @TT "qlc"@.
     
     Text 
      The first example computes the quasidegree set of 
      $H_{\bf m}^0(R/I)\oplus H_{\bf m}^1(R/I)$ where $I$ is the toric ideal associated to the 
      matrix $A$.

     Example
      A = matrix{{1,1,1,1},{0,1,5,11}}
      R = QQ[a..d]
      R = toGradedRing(A,R)
      I = toricIdeal(A,R)
      M = R^1/I
      quasidegreesLocalCohomology M

     Text
      The above example gives that the quasidegrees of the non-top local cohomology of $M$ are 
      (4,9), (3,9), (2,4), and (3,4).  We can see that these all come from the first local 
      cohomology module.
     
     Example
      quasidegreesLocalCohomology(1,M)
      
     Text
      The next example shows a module whose quasidegree set of its second local cohomology 
      module at the irrelevant ideal, is a line.
      
     Example
      A = matrix{{1,1,1,1,1},{0,0,1,1,0},{0,1,1,0,-2}}
      R = QQ[a..e]
      R = toGradedRing(A,R)
      I = toricIdeal(A,R)
      M = R^1/I
      quasidegreesLocalCohomology(2,M)
     
     Text
      The above example gives that the quasidegrees of the second local cohomology module of $M$
      at the irrelevant ideal is the complex parameterized line (0,0,1)+$t\bullet$(1,0,-2).
///


----------------------------------
-- Documentation exceptionalSet --
----------------------------------

doc ///
    Key
     exceptionalSet
     (exceptionalSet,Matrix)
    Headline
     returns the exceptional set of a matrix
    Usage
     exceptionalSet A
    Inputs
     A: 
      a $d\times n$ integer matrix
    Outputs
     :List 
      a list that indexes the exceptional parameters of $A$
    Description
     Text
      This method takes a $d\times n$ integer matrix $A$ and computes the exceptional parameters
      of $A$.  The exceptional parameters of $A$ are the $\beta\in{\mathbb C}^d$ such that the 
      rank of the hypergeometric system $H_\beta(A)$ does not take the expected value.  The 
      exceptional parameters of $A$ are indexed by a list of pairs $(v,F)$ where $v$ is a 
      vector and $F$ is a list of vectors.  The pair $(v,F)$ represents the plane 
      $v+span_{\mathbb C} F$.  The set of exceptional parameters of $A$ is the union of all such
      planes given by the pairs $(v,F)$.
      
     Example
      A=matrix{{1,1,1,1},{0,1,5,11}}
      exceptionalSet A
      
     Text
      Thus, when $\beta$=(4,9), (3,9), (2,4), or (3,4), the rank of the hypergeometric system 
      $H_\beta(A)$ is higher than expected.
///


------------------------------------
-- Documentation removeRedundancy --
------------------------------------

doc ///
    Key
     removeRedundancy
     (removeRedundancy,List)
    Headline
     removes redundancies from a list of planes
    Usage
     removeRedundancy L
    Inputs
     L:
      of pairs indexing affine planes
    Outputs
     :List
      with no redundancies in L.
    Description
     Text
      The method takes a list of pairs that indexes planes and removes redundancies in the
      list.  By a redundancy, we mean when one plane in the list is contained in another
      plane.
     
     Example
      R = QQ[x,y,z]
      I = ideal(x*y,y*z)
      Q = quasidegrees(R^1/I)
      
     Text
       The two pairs in {\tt Q} each correspond to the complex plane so there is a redundancy.
       
     Example
      removeRedundancy Q
///

--================================================--
--******************** TESTS *********************--
--================================================--

--------------------------------------
-- TEST quasidegreesLocalCohomology --
--------------------------------------

-- This test checks that the quasidegrees given actually are
-- parameters b where the rank of the associated hypergeometric system
-- is higher than expected.

TEST ///
needsPackage"Dmodules"
A = matrix{{1,1,1,1},{0,1,5,11}}
R = toGradedRing(A,QQ[a..d])
I = toricIdeal(A,R)
S = quasidegreesLocalCohomology R^1/I
T = {}; for i to #S-1 do T=T|{(S_i)_0}
for i to #T-1 do assert(holonomicRank(gkz(A,{0,0}))<holonomicRank(gkz(A,entries(T_i))))
///

-------------------------
-- TEST exceptionalSet --
-------------------------

-- This test checks that the exceptional set are parameters where the rank
-- of the associated hypergeometric system is higher than expected.

TEST ///
needsPackage"Dmodules"
A = matrix{{1,1,1,1},{0,1,5,11}}
E = exceptionalSet A
T = {}; for i to #E-1 do T=T|{(E_i)_0}
for i to #T-1 do assert(holonomicRank(gkz(A,{0,0}))<holonomicRank(gkz(A,entries(T_i))))
///





end

--================--
-- End of Package --
--================--


uninstallPackage"Quasidegrees"
restart
installPackage"Quasidegrees" 
check "Quasidegrees"
needsPackage"Quasidegrees"

