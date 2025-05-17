newPackage(
          "Brackets",
          Version => "0.1",
          Date => "April 15, 2025",
          Headline => "Brackets, Grassmann-Cayley Algebra, and Projective Geometry",
          Authors => {
	      { Name => "Dalton Bidleman", Email => "deb0036@auburn.edu", HomePage => ""},
	      { Name => "Timothy Duff", Email => "tduff@missouri.edu", HomePage => "https://timduff35.github.io/timduff35/"},
	      { Name => "Jack Kendrick", Email => "jackgk@uw.edu", HomePage => ""},
	      { Name => "Michael Zeng", Email => "zengrf@uw.edu", HomePage => ""}		      
	      },
	  PackageImports => {},
          AuxiliaryFiles => false,
          DebuggingMode => false,
	  Keywords => {"Projective Algebraic Geometry"}
          )

export {"Bracket", "AbstractGCRing", "bracketRing", "BracketRing", "GCAlgebra", "normalForm", "gc", "toBracketPolynomial", "GCExpression"}

-* Code section *-

-- easy inputting of brackets and other helper functions
ZZ ZZ := (a, b) -> [a, b]
Array ZZ := (a, b) -> a | [b]
ZZ Array := (a, b) -> [a] | b
RingElement RingElement := (a, b) -> [a, b]
Array RingElement := (a, b) -> a | [b]
RingElement Array := (a, b) -> [a] | b
increment = L -> L/(l-> l+1)
sgn = sigma -> ( -- sign of a permutation
    n := length sigma;
    product(0..n-1, i -> product(i..n-1, j -> if sigma#i > sigma#j then (-1) else 1))
    )

-- AbstractGCRing is a parent class for BracketRing and GCAlgebra
AbstractGCRing = new Type of HashTable
net AbstractGCRing := G -> error "not implemented"
ring AbstractGCRing := G -> G#ring
use AbstractGCRing := G -> use ring G
AbstractGCRing Array := (G, A) -> (
    R := ring G;
    new AbstractGCRing from {ring => R A, cache => new CacheTable from {}}
    )

-- class declaration and constructors for BracketRing
BracketRing = new Type of AbstractGCRing

bracketRing = method(Options => {Strategy => GroebnerBasis, CoefficientRing => QQ,Variables=>{}})
bracketRing AbstractGCRing := G -> error "not implemented"
bracketRing (VisibleList, ZZ) := o -> (vectorSymbols, d) -> (
    n := length vectorSymbols;
    if not (n >= d) then error("The first argument n in bracketRing(n, d) (representing the number of rows) is assumed to be at least the second argument d (representing the number of columns)");
    x := symbol x;
    R := o.CoefficientRing[x_(1,1)..x_(n,d)];
    X := matrix for i from 1 to n list for j from 1 to d list x_(i,j);
    nBrackets := rsort(sort \ subsets(vectorSymbols, d)); -- important for "Tableux order"
    bracketIndices := rsort(sort \ subsets(#vectorSymbols, d));
    minorsX := apply(bracketIndices, R -> det X^R);
    y := symbol y; 
    bracketVariables := apply(nBrackets, S -> y_("["|fold(S, (a, b) -> toString(a)|toString(b))|"]"));
    S := o.CoefficientRing[gens R, bracketVariables, MonomialOrder => {Eliminate(numgens R), GRevLex}]; -- important for "Tableaux order"
    lookupTable := new HashTable from apply(binomial(n, d), i -> increment bracketIndices#i => (gens S)#(numgens R+i));
    I := ideal apply(minorsX, bracketVariables, (m, b) -> sub(m, S) - b_S);
    ret := new BracketRing from {numrows => n, numcols => d, ring => S, ideal => I, table => lookupTable, cache => new CacheTable from {}};
    local G;
    if o#Strategy === GroebnerBasis then (
	-- TODO: allow computing with SubalgebraBases instead of Groebner bases, or pre-cooked Grassmannian GB (needs correct monomial order...)
	G = groebnerBasis I;
	ret.cache#gb = G;
	ret.cache#syz = selectInSubring(1, G);
	) 
    else error "Strategy option not recognized";
    ret
    )
bracketRing (ZZ, ZZ) := o -> (n, d) -> bracketRing(toList(1..n), d, o)
-- printing for BracketRing
net BracketRing := B -> net((symbol B)_(B#numrows, B#numcols))
-- getters for BracketRing
numrows BracketRing := B -> B#numrows 
numcols BracketRing := B -> B#numcols 
ideal BracketRing := B -> B#ideal
bracketRing BracketRing := o -> B -> B

ZZ _ AbstractGCRing := (k, G) -> sub(k, ring G)
matrix BracketRing := o -> B -> transpose genericMatrix(ring B,numcols B, numrows B)

-- class declaration and constructors for GCAlgebra
GCAlgebra = new Type of AbstractGCRing
gc = method(Options => {Strategy => GroebnerBasis,CoefficientRing => QQ})

gc (VisibleList, ZZ) := o -> (vectorSymbols, d) -> (
    n := # vectorSymbols;
    (inputMode, vectorVariables) := (
	if all(vectorSymbols, s -> instance(s, IndexedVariable) or instance(s, Symbol)) then (ZZ, vectorSymbols)
	else if all(vectorSymbols, s -> instance(s, ZZ)) and n == # unique vectorSymbols then (a := symbol a; (RingElement, apply(vectorSymbols, s -> a_s)))
	else error "incorrect input"
	);
    Bnd := bracketRing(toList vectorSymbols, d, Strategy => o.Strategy, CoefficientRing => o.CoefficientRing);
    nBrackets := subsets(n,d+1);
    R := (ring Bnd)[vectorVariables, SkewCommutative => true];
    S := R/(ideal apply(nBrackets, S -> product(S, s -> (vectorVariables#s)_R)));
    new GCAlgebra from {bracketRing => Bnd, ring => S, "num_pts" => #vectorSymbols}
    )

bracketRing GCAlgebra := o -> Gnd -> Gnd#bracketRing
gens AbstractGCRing := o -> G -> gens ring G
net GCAlgebra := Gnd -> (
    R := ring Gnd;
    n := numgens R;
    "Grassmann-Cayley Algebra generated by 1-extensors " | toString(R_0) | ".." | toString(R_(n-1))
    )
numgens AbstractGCRing := Gnd -> numgens ring Gnd

GCExpression = new Type of HashTable
Bracket = new Type of GCExpression

-- pretty printing
net GCExpression := b -> (
    r := b#RingElement;
    B := bracketRing b;
    xs := drop(gens ring r, numcols B * numrows B);
    rStr := toString r;
    replace("y_","",rStr)
    )
Array _ AbstractGCRing := (A, R) -> (
    assert(#A == 1); -- For now, this function assumes the first argument A is a single-element, doubly-nested array of the form [[i1 i2 ... id]]
    A0 := A#0;
    B := bracketRing R;
    if not(B#numcols == #A0) then error "not enough symbols in bracket";
    A1 := sort toList A0;
    if any(A1, s -> instance(s, Symbol)) then A1 = apply(A1, g -> (g_R)#RingElement);
    assert(
	-- checking that the input is valid
	-- TODO: include better error messages
	(instance(R, GCAlgebra) and all(A1, x -> instance(x, ambient ring R))) or 
	all(A1, i -> (instance(i, ZZ) and i >= 1 and i <= B#numrows))
	);
    rowSet := first select(1, keys B#table, k -> toList A1 == k);
    new Bracket from {RingElement => (sgn A0) * B#table#rowSet, ring => R}
)

coefficients GCExpression := o -> a -> (
    G := ring a;
    if instance(G, BracketRing) then (
	R1 := ring G;
	R2 := coefficientRing R1;
	(m, c) := coefficients sub(a#RingElement, (coefficientRing R2)[gens R1][gens R2]);
	(m, matrix(G, apply(flatten entries c, e -> {e_G})))
	)
    else error "not implemented"
)

bracketRing GCExpression := o -> b -> bracketRing ring b
commonRing (GCExpression, GCExpression) := (b1, b2) -> (
    -- returns: either GCAlgebra or BracketRing in which b1 & b2 both make s
    (G1, G2) := (ring b1, ring b2);
    if (G1 === G2 or G2 === bracketRing G1) then G1 else if G1 === bracketRing G2 then G2 error "Common abstract GC ring not found"
    )
degree GCExpression := A -> degree A#RingElement
ring GCExpression := b -> b#ring

RingElement _ AbstractGCRing := (b, R) -> new GCExpression from {RingElement => b, ring => R}

-- piggybacking on operators for the associated RingElement
terms GCExpression := b -> (bRTerms := terms(b#RingElement);
     for term in bRTerms list term_(ring b))


GCExpression + GCExpression := (b1, b2) -> (
    R := commonRing(b1, b2);
    b := b1#RingElement + b2#RingElement;
    b_R
    )
GCExpression * GCExpression := (b1, b2) -> (
    R := commonRing(b1, b2);
    b := b1#RingElement * b2#RingElement;
    bR := b_R;
    bRTerms := terms bR;
    if (instance(R, GCAlgebra) and (all(bRTerms, t -> (isTopDegree t or isBottomDegree t)))) then sum(bRTerms, t -> t_(bracketRing R)) else bR
    )
GCExpression - GCExpression := (b1, b2) -> (
    R := commonRing(b1, b2);
    b := b1#RingElement - b2#RingElement;
    b_R
    )
GCExpression ^ ZZ := (b, k) -> new GCExpression from {RingElement => (b#RingElement)^k, ring => ring b}
RingElement * GCExpression := (c, b) -> new GCExpression from {RingElement => c * b#RingElement, ring => ring b}
GCExpression * RingElement := (b, c) -> new GCExpression from {RingElement => c * b#RingElement, ring => ring b}
Number * GCExpression := (c, b) -> new GCExpression from {RingElement => c * b#RingElement, ring => ring b}
GCExpression * Number := (b, c) -> new GCExpression from {RingElement => c * b#RingElement, ring => ring b}

toBracketPolynomial = method();
toBracketPolynomial(RingElement, BracketRing) := (f, G) -> ( --input: polynomial, bracketring
    I := G#ideal;
    (f % I)_G
)


isExtensor = method()
isExtensor GCExpression := A -> (
    assert(instance(ring A, GCAlgebra));
    1 == length terms someTerms(A#RingElement, 0, 2)
    )
isTopDegree = method()
isTopDegree GCExpression := A -> (
    assert(instance(ring A, GCAlgebra));
    (k, d) := (first degree A, numcols bracketRing A);
    (isHomogeneous A#RingElement and k == d)
    )
isBottomDegree = method()
isBottomDegree GCExpression := A -> (
    assert(instance(ring A, GCAlgebra));
    (k, d) := (first degree A, numcols bracketRing A);
    (isHomogeneous A#RingElement and k == 0)
    )
extensorSupportIndices = method()
extensorSupportIndices GCExpression := A -> (
    assert(isExtensor A);
    vectorExponents := take(first exponents A#RingElement, {0, (ring A)#"num_pts" - 1});
    increment positions(vectorExponents, p -> p == 1)
    );
extensorToBracket = method()
extensorToBracket GCExpression := A -> (
    assert(instance(ring A, GCAlgebra));
    elemA := A#RingElement;
    Bnd := bracketRing A;
    local ret;
    if elemA == 0 then ret = 0 else (
	assert(first degree elemA == numcols bracketRing A);
	S := extensorSupportIndices A;
	ret = (leadCoefficient elemA) * sub(Bnd#table#S, ring elemA)
	);
    ret
    );

shuffleProduct = (A, B) -> (
    assert(isExtensor A and isExtensor B);
    Bnd := bracketRing A;
    (j, k, d) := (first degree A, first degree B, numcols bracketRing Bnd);
    if not (j+k >= d) then error "shuffle product undefined";
    P1 := permutations toList(0..d-k-1);
    P2 := permutations toList(d-k..j-1);
    shuffles := flatten flatten apply(P1, p1 -> apply(P2, p2 -> {p1 | p2, p2 | p1}));
    elemA := A#RingElement;
    suppA := support elemA;
    sum(shuffles, sigma -> (
	    bracketCoeff := extensorToBracket((leadCoefficient A#RingElement) * product(0..d-k-1, i -> suppA#(sigma#i)) * B);
	    mon := product(d-k..j-1, i -> suppA#(sigma#i));
	    ret := (sgn sigma) * bracketCoeff * mon;
	    ret
	    )
	)
    )

GCExpression _ GCAlgebra := (b, G) -> if (not instance(G, BracketRing) and ring b === G) then b else error "GCExpression belongs to a different ring"

GCExpression _ BracketRing := (b, B) -> (
    assert(B === bracketRing b);
    bBracket := if (isTopDegree b) then sum(terms b, bterm -> lift(extensorToBracket bterm, ring B)) else if (isBottomDegree b) then lift(b#RingElement, ring B) else error "must be an extensor of step 0 or d";
    bBracket_B
    )


GCExpression ^ GCExpression := (f, g) -> (
    G := ring f;
    assert(instance(G, GCAlgebra) and G === ring g);
    B := bracketRing G;
    fExtensors := apply(terms f#RingElement, t -> t_G);
    gExtensors := apply(terms g#RingElement, t -> t_G);
    result := (sum flatten apply(fExtensors, fE -> apply(gExtensors, gE -> shuffleProduct(fE, gE))))_G;
    if ((isBottomDegree result or isTopDegree result) and isExtensor result) then result = result_B;
    result
)
GCExpression ^ RingElement := (f, g) -> f ^ (g_(ring f))
RingElement ^ GCExpression := (f, g) -> (f_(ring g)) ^ g

normalForm = method()
normalForm GCExpression := b -> (
    assert(instance(ring b, BracketRing));
    B := bracketRing b;
    I := ideal bracketRing b;
    r := b#RingElement;
    nf := (r % I);
    nf_B
    )    

factor GCExpression := o -> g -> (
    F := factor g#RingElement;
    G := ring g;
    apply(toList F, fi -> (fi#0^(fi#1))_G)
    )

GCMatrix = new Type of HashTable

matrix (AbstractGCRing, List) := o -> (B, L) -> (
    listM := for i from 0 to (#L-1) list(apply(L#i, t-> t#RingElement));
    new GCMatrix from {matrix => matrix listM, bracketRing => B }
    )

net GCMatrix := M -> (
    L := entries  M#matrix;
    B := M#bracketRing;
    T := applyTable(L, t->t_B);
    netList(toList T,HorizontalSpace=>1, Boxes=>{false,{0, length T_0}}))

GCMatrix + GCMatrix := (M1, M2) -> (
    B := M1#bracketRing;
    M:=M1#matrix + M2#matrix;
    new GCMatrix from {matrix => M, bracketRing => B}
    )

GCMatrix * GCMatrix := (M1, M2) -> (
    B := M1#bracketRing;
    M:=M1#matrix *  M2#matrix;
    new GCMatrix from {matrix => M, bracketRing => B}
    )

RingElement * GCMatrix := (r, M) -> (
    new GCMatrix from {matrix => (r * M#matrix), bracketRing => M#bracketRing})

GCMatrix * RingElement := (M, r) -> (
    new GCMatrix from {matrix => (r * M#matrix), bracketRing => M#bracketRing})

ring GCMatrix := M -> M#bracketRing

GCMatrix _ Sequence := (M, twoIndices) -> ((M#matrix)_twoIndices)_(ring M)

det GCMatrix := o -> M -> (det M#matrix)_(M#bracketRing)

GCExpression * GCMatrix := (e, M) -> (
    mat := M#matrix;
    r := e#RingElement;
    new GCMatrix from {matrix => r*mat, bracketRing => M#bracketRing}
    )

GCMatrix * GCExpression := (M, e) -> (
    mat := M#matrix;
    r := e#RingElement;
    new GCMatrix from {matrix => r*mat, bracketRing => M#bracketRing}
    )

transpose GCMatrix := M -> (
    new GCMatrix from {matrix => transpose M#matrix, bracketRing => M#bracketRing}
    )

GCMatrix_ZZ := (M, n) -> (
    new GCMatrix from {matrix =>matrix(M#matrix)_n, bracketRing => M#bracketRing}
    )

GCMatrix^ZZ := (M, n) -> (
    new GCMatrix from {matrix =>matrix(M#matrix)^n, bracketRing => M#bracketRing}
    )

GCMatrix_List := (M, L) -> (
    new GCMatrix from {matrix =>matrix(M#matrix)_L, bracketRing => M#bracketRing}
    )
 
 GCMatrix^List := (M, L) -> (
    new GCMatrix from {matrix =>matrix(M#matrix)^L, bracketRing => M#bracketRing}
    )

entries GCMatrix := M -> (
    L := entries  M#matrix;
    B := M#bracketRing;
    T := applyTable(L, t->t_B))

undocumented {AbstractGCRing, (net, GCAlgebra)}

beginDocumentation()

doc ///
Key
  Brackets
Headline
  Brackets, Grassmann-Cayley Algebra, and Projective Geometry
Description
  Text
      Fix integers $n \ge d \ge 1,$ and let $X = (x_{i j})$ be an $n\times d$ matrix of distinct variables in the polynomial ring $k [x_{i j}]$ over a fixed field $k.$    
      Think of each row of $X$ is a point in the projective space $\mathbb{P}^{d-1}$ of dimension $(d-1)$ over $k,$ so that $X$ as represents a configuration of $n$ points in this projective space.
      Many interesting geometric properties of this point configuration can be expressed in terms of the maximal minors of $X.$
      
      For notational convenience, it is common to write these minors in bracket notation.
      A bracket is an expression $[\lambda_1 \lambda_2 \ldots \lambda_d]$ representing the minor of $X$ whose rows are given by indices $1\le \lambda_1 < \lambda_2 < \ldots < \lambda_d \le n.$
    
      Formally, we may consider the map of polynomial rings
      $$\psi_{n,d} : k \left[ [\lambda_{i_1} \cdots \lambda_{i_d}] \mid 1 \le i_1 < \ldots < i_d \le n \right] \to k [X],$$
      $$ [\lambda_{i_1} \cdots \lambda_{i_d}] \mapsto \det \begin{pmatrix} x_{i_1, 1} & \cdots & x_{i_1, d} \\ \vdots & & \vdots & \\ x_{i_d 1} & \cdots & x_{i_d d}\end{pmatrix}. $$
      
      The classical bracket ring $B_{n,d}$ is the image of this map.
      This is the homogeneous coordinate ring of the Grassmannian of $(n-1)$-dimensional planes in $\mathbb{P}^{d-1}$ under its Plücker embedding.
Acknowledgement
  We thank Thomas Yahl for helpful contributions, and the organizers of the 2023 Macaulay2 workshop in Minneapolis, IMA staff, and acknowledge support from the National Science Foundation grant DMS 2302476.
References
  Sturmfels, Bernd. {\it Algorithms in invariant theory}. Springer Science & Business Media, 2008.
///

doc ///
Key
  BracketRing
Description
  Text
    An object of class BracketRing represents the bracket ring $B_{n,d}$.
    For example, let $n=4, d=2,$ so that
      $$X=\begin{pmatrix}
        x_{1,1}&x_{1,2}\\
        x_{2,1}&x_{2,2}\\
        x_{3,1}&x_{3,2}\\
        x_{4,1}&x_{4,2}\\
        \end{pmatrix}.$$
      There are $6=\binom{4}{2}$ brackets, and the matrix $X$ represents a configuration of $6$ points on the projective line $\mathbb{P}^1.$
      These brackets are not algebraically independent, as they satisfy the quadratic Plücker relation,
      $$
      [1 2] [3 4] - [1 3] [2 4] + [1 4] [2 3] = 0.
      $$
      Some basic syntax for working with objects of class BracketRing is illustrated in the documentation page @TO bracketRing@.
///

doc ///
Key
  bracketRing
  (bracketRing, AbstractGCRing) 
  (bracketRing, BracketRing)    
  (bracketRing, GCAlgebra)      
  (bracketRing, GCExpression)   
  (bracketRing, VisibleList, ZZ)
  (bracketRing, ZZ, ZZ)         
Headline
  Constructor for bracket rings
Usage
  B = bracketRing(n, d)
  B = bracketRing(vectorSymbols, d)
  B = bracketRing B'
  B = bracketRing G
Inputs
  B':BracketRing
  G:GCAlgebra
  vectorSymbols:List
  n:ZZ
  d:ZZ
Outputs
  B:BracketRing
Description
  Text
    To construct the bracket ring $B_{n,d}$ it is enough to specify two integers.
    In that case, each bracket will contain $n$ symbols which are integers between $1$ and $d.$
  Example
    B = bracketRing(6, 3)
    T = [1 4 5]_B * [1 5 6]_B * [2 3 4]_B
  Text
    One may also provide @ ofClass{VisibleList} @ of $n$ symbols.
  Example
    B2 = bracketRing(a..f, 3)
  Text
    Additionally, brackets can be interpreted as the top-degree elements of the Grassmann-Cayley algebra.
  Example
    G = gc(a..f, 3)
    B3 = bracketRing G
  Text
    See also @TO BracketRing@.
///

doc ///
Key 
 GCAlgebra
Description
 Text
  An object of class GCAlgebra represents a Grassmann-Cayley algebra. 
  The Grassmann-Cayley algebra may be viewed as an algebra of linear subspaces of $\mathbb{P}^{d−1}.$ 
  In this algebra, there are two operations which correspond to the join and meet of subspaces. 
  We denote these operators by * and ^, respectively. 
  The first operator is simply multiplication in a skew-commutative polynomial ring $\mathbb{C}\langle a_1, . . . , a_n\rangle.$ 
  An algebraic formula for the meet operator is more complicated, but it can be defined using the so-called shuffle product. 
  
  As a $k-$vector space, the Grassmann-Cayley algebra has a direct-sum decomposition
  $$\oplus_{k=0}^d\Lambda^k(a_1, \ldots, a_n)$$
  where $\Lambda^k(a_1,\ldots, a_n)$ is the vector space of {\it extensors} of the form $a_{i_1}\cdots a_{i_k}.$
  We may identify $\Lambda^d(a_1, \ldots, a_n)\cong B_{n,d}$ with the @TO BracketRing @.
///

doc ///
Key
 gc
 (gc, VisibleList, ZZ)
Headline
 Constructor for Grassmann-Cayley algebras.
Usage
 G = gc(vectorSymbols, d)
Inputs
 vectorSymbols:List
 d:ZZ
Outputs
 G:GCAlgebra
Description
 Text
  To construct a Grassmann-Cayley algebra specify a @ ofClass{VisibleList} @ of $n$ symbols, representing $n$ points in a projective space $\mathbb{P}^{d-1}$, and the integer $d.$
 Example
  G = gc(a .. f, 3)
 Text
  See also @TO GCAlgebra@.
///


doc ///
Key 
  Bracket
Description
  Text
    In a BracketRing, a Bracket represents a maximal minor of the matrix of coordinates of a configuration of points in a projective space. The example below illustrates this for a configuration of 5 points in the projective space $\mathbb{P}^2$.
  Example
    B = bracketRing(5,3)
    [1 2 3]_B
    X = matrix B
    toBracketPolynomial(det X^{0,1,2},B)
  Text
    See also @TO BracketRing@ and @TO toBracketPolynomial@.

///



doc ///
Key
  toBracketPolynomial
  (toBracketPolynomial, RingElement, BracketRing)
Headline
  Represent an invariant polynomial as a polynomial in brackets
Usage
  g = toBracketPolynomial(f,B)
Inputs
  f:RingElement
  B:BracketRing
Outputs
  g:GCExpression
Description
  Text
    A polynomial invariant under the action of the special linear group may be represented by a polynomial in brackets. See @TO Bracket@ for more details. Groebner basis methods allow one to compute such a representation. Below is a simple example.
  Example
    B = bracketRing(3, 2)
    X = matrix B
    f = (det X^{1,2}) - (det X^{0,2}) + (det X^{0,1})
    toBracketPolynomial(f,B)
  Text
    Such a representation is not unique. It may be checked that two bracket polynomials are equal through their normal form with respect to a Groebner basis. See @TO normalForm@ for a further explanation. 

    See also @TO BracketRing@.
///

doc ///
Key
  normalForm
  (normalForm, GCExpression)
Headline
  Represent a bracket polynomial in a normal form with respect to a Groebner basis
Description
  Text
    The relations between brackets generate an ideal, which are the classic Plücker relations. A Groebner basis for this ideal allows one to represent a polynomial in brackets in a unique normal form. 
  Example
    B = bracketRing(4,2)
    f = [1 2]_B * [3 4]_B - [1 3]_B * [2 4]_B + [1 4]_B * [2 3]_B
    normalForm(f)
  Text
    See also @TO Bracket@, @TO toBracketPolynomial@, and @TO bracketRing@
///

doc ///
Key
  GCExpression
Description
  Text
    A general data type for representing elements of bracket rings and Grassmann-Cayley algebras.
    GC expressions can be assembled into matrices, and they support a number of the usual arithmetic operations: addition, multiplication, and scalar multiplication.
    
    Multiplication on the Grassmann-Cayley algebra is the usual exterior product.
    This represents the span, or join, of linear subspaces in a given vector space.
    
    The Grassmann-Cayley algebra is also endowed with a "shuffle product", representing the intersection or meet of linear subspaces.
    This is implemented in @TO (symbol ^, GCExpression, GCExpression)@.
///

doc ///
Key
  (symbol ^, GCExpression, GCExpression)
Headline
  Shuffle product in the Grassmann-Cayley Algebra
Usage
  f = g ^ h;
Inputs
  g:GCExpression
  h:GCExpression
Outputs
  f:GCExpression
Description
  Text
    Let $V$ be a vector space of dimension $d$ over a field $\mathbb{F}.$
    We recall the exterior algebra,
    \[
    \Lambda (V) = \displaystyle\bigoplus_{k=0}^d \Lambda^k (V),
    \]
    a set which forms an algebra with the usual addition and exterior product.
    The {\it Grassmann-Cayley algebra} is obtained by endowing this set with an additional shuffle product, defined below.

    The linear span of $k$ independent vectors $a_1, \ldots , a_k\in V$ is represented by the {\it extensor} $a_1 \cdots a_k \in \Lambda^k (V)$, where the product is the usual exterior product.
    If we fix a basis $\{ e_1, \ldots , e_d \}$ for $V,$ then each exterior power $\Lambda^k (V)$ has a basis given by the extensors of the form $e_{i_1} \vee \cdots \vee e_{i_k}.$
    We identify the extensor $a_1 \cdots a_d$ with the bracket $[a_1, \ldots , a_d]$ (see @TO BracketRing @.)
    
    Let $A = a_1 a_2 \cdots a_j$ and $B = b_1 b_2 \cdots b_k$ be extensors with $j+k\ge d.$
    The {\it shuffle product} of $A$ and $B$ represents the intersection of the subspaces represented by $A$ and $B.$
    is defined by the formula
    \[
    A \vee B = 
    \displaystyle\sum_{\sigma \mid \text{is shuffle}} 
    \operatorname{sgn}(\sigma ) [a_{\sigma (1)}, \ldots , a_{\sigma (d-k)}, b_1, \ldots , b_k],
    \]
    where the sum is taken over {\it shuffle permutations} with respect to the split $(d-k, j + k -d).$
    These are the permutations $\sigma : [j] \to [j]$ satisfying $\sigma (1) < \sigma (2) < \ldots  < \sigma (d-k)$ and $\sigma (d-k +1) < \sigma (d-k+2) < \cdots < \sigma (j).$
    Extending $\mathbb{F}$-linearly, the shuffle product defines an associative, skew-commutative multiplication.
    
    The exterior and shuffle products are implemented using the operators @TO symbol * @ and  @TO symbol ^ @, respectively.
    When using Grassmann-Cayley algebras to prove theorems involving point configurations in $\mathbb{P} (V)$, it is beneficial to work with indeterminate points, rather than fixed elements of $V.$
    Internally, we represent these indeterminate points with variables in a skew-commutative polynomial ring, $\mathbb{F}< a_1, \ldots , a_n>$ (see @TO SkewCommutative @.) 

    In the example below, three lines spanned by six distinct points in $\mathbb{P}^2$ in the Grassmann-Cayley algebra, as well as the intersection of these three lines.
    In the latter case, we obtain an element of the bracket ring $B_{3,6}$ (see @TO BracketRing @.)
  Example
    G = gc(a..f, 3)
    lab = (a*b)_G
    lcd = (c*d)_G;
    lef = (e*f)_G;
    lab ^ lcd ^ lef
  Text
    See also @TO GCAlgebra@, @TO GCExpression@.
///

doc ///
Key
  (symbol _, RingElement, AbstractGCRing)
Headline
  Substituting ring elements into bracket rings and GC algebras
Usage
  a = b_R;
Inputs
  b:RingElement
  R:AbstractGCRing
Outputs
  a:GCExpression
Description
  Text
    This element is required in order to interpret various symbols as elements of the Grassmann-Cayley algebra or its associated bracket ring.
///

doc ///
Key
  (symbol _, GCExpression, BracketRing)
Headline
  Substituting top-degree Grassmann-Cayley elements into the bracket ring
Usage
  a = b_R;
Inputs
  b:GCExpression
  R:BracketRing
Outputs
  a:GCExpression
Description
  Text
    Interpreting bracket polynomials as functions on $\Lambda^d (V)$ (see @TO (symbol ^, GCExpression, GCExpression)@ for notation), we may convert an extensor $a_1 \wedge \cdots \wedge a_d$ to the bracket $[a_1, \ldots , a_d].$
///


TEST ///
-* Sturmfels Example 3.1.10 *-
B = bracketRing(6, 3)
T = [1 4 5]_B * [1 5 6]_B * [2 3 4]_B
n = normalForm T 
assert(net n == "[256]*[145]*[134]-[356]*[145]*[124]+[456]*[145]*[123]")
-- Note: this is the same normal form Sturmfels gives, but monomials and their exponents are reverse-sorted
///

TEST ///
-* Sturmfels Example 3.3.3 *-
G = gc(a..f, 3)
A = (a * d)_G
B = (b * e)_G
AB = A ^ B 
C = (c * f)_G
D = AB ^ C
-- Note: Output "2*[bde]*[acf]-2*[cdf]*[abe]" is consistent with the book's answer up to sorting and sign.
assert(net D == "2*[bde]*[acf]-2*[cdf]*[abe]")
///

TEST ///
-* Desargues' Theorem*-
G = gc(a..f,3)
abLine = (a * b)_G
deLine = (d * e)_G
bcLine = (b * c)_G
efLine = (e * f)_G
acLine = (a * c)_G
dfLine = (d * f)_G
pt1 = abLine ^ deLine
pt2 = bcLine ^ efLine
pt3 = acLine ^ dfLine
linePerspective = pt1 * pt2 * pt3
adLine = (a * d)_G
beLine = (b * e)_G
cfLine = (c * f)_G
pointPerspective =  adLine ^ beLine ^ cfLine
assert(net pointPerspective == "2*[bde]*[acf]-2*[cdf]*[abe]")
(n1, n2) = (normalForm pointPerspective, normalForm linePerspective);
(f1, f2) = (factor n1, factor n2)
assert(net f1#0 == "[bdf]*[ace]-[bef]*[acd]-[cdf]*[abe]-[def]*[abc]")
assert(net f2#2 == "[bdf]*[ace]-[bef]*[acd]-[cdf]*[abe]-[def]*[abc]")
///


TEST ///
--simple colinearity condition
B = bracketRing(a..c,3)
X = matrix B
f = toBracketPolynomial(det X,B)
assert(net f == "[abc]")
///


TEST ///
--condition for 6 points to lie on a conic
B = bracketRing(6,3)
X = matrix B
C = fold(apply(0..5,i->basis(2,ring X,Variables => (entries X)#i)),(a,b)->a||b)
D = det C
f = toBracketPolynomial(D,B)
assert(net f == "-[456]*[236]*[135]*[124]+[356]*[246]*[145]*[123]")
///


TEST ///
--Sturmfels 3.2.10 example
B = bracketRing(toList(a..f),2)
X = matrix B

--polynomial from Example 3.2.10 in Sturmfels
F = -X_(0,0)*X_(1,0)*X_(2,0)*X_(3,1)*X_(4,1)*X_(5,1) - X_(0,0)*X_(1,0)*X_(2,1)*X_(3,0)*X_(4,1)*X_(5,1) + X_(0,0)*X_(1,0)*X_(2,1)*X_(3,1)*X_(4,0)*X_(5,1) + X_(0,0)*X_(1,0)*X_(2,1)*X_(3,1)*X_(4,1)*X_(5,0) + X_(0,0)*X_(1,1)*X_(2,0)*X_(3,0)*X_(4,1)*X_(5,1) - X_(0,0)*X_(1,1)*X_(2,1)*X_(3,1)*X_(4,0)*X_(5,0) + X_(0,1)*X_(1,0)*X_(2,0)*X_(3,0)*X_(4,1)*X_(5,1) - X_(0,1)*X_(1,0)*X_(2,1)*X_(3,1)*X_(4,0)*X_(5,0) - X_(0,1)*X_(1,1)*X_(2,0)*X_(3,0)*X_(4,0)*X_(5,1) - X_(0,1)*X_(1,1)*X_(2,0)*X_(3,0)*X_(4,1)*X_(5,0) + X_(0,1)*X_(1,1)*X_(2,0)*X_(3,1)*X_(4,0)*X_(5,0) + X_(0,1)*X_(1,1)*X_(2,1)*X_(3,0)*X_(4,0)*X_(5,0) 
F' = toBracketPolynomial(F,B)
assert(net F' == "-[cf]*[be]*[ad]-[df]*[be]*[ac]+[ef]*[bd]*[ac]+[df]*[ce]*[ab]-[ef]*[cd]*[ab]")
///


TEST ///
--Sturmfels 3.2 problem 2 (4 points in P^1 that lie on a cubic, two must be equal)
B = bracketRing(4,2)
X = matrix B
P = matrix apply(4,i->flatten entries basis(3,ring B,Variables=>(entries X)#i))
f = det P--this is the normal form of the product [1 2]_B * [1 3]_B * [1 4]_B * [2 3]_B * [2 4]_B * [3 4]_B
assert(net toBracketPolynomial(f,B) == "[34]*[24]^2*[13]^2*[12]-[34]^2*[24]*[13]*[12]^2")
///


TEST ///
--Sturmfels 3.1 problem 2 (determinant of a GC matrix)
B = bracketRing(6,3)
M = matrix(B, for i from 4 to 6 list {[1 2 i]_B, [1 i 3]_B, [i 2 3]_B})
normalForm (det M)
assert(net normalForm (det M) == "-[456]*[123]^2")
///

TEST ///
-* Pascal's Theorem*-
G = gc(a .. f, 3) -- generate the GC Algebra generated by the 1-extensors a..f 
gens --G create a list containing just the extensors
B = bracketRing G; -- define the bracketRing of G
X = matrix B; -- create the matrix of the bracketRing
C = fold(apply(0..5, i-> basis(2, ring X, Variables => (entries X)#i)), (a,b) -> a||b); -- Matrix with rows corresponding to six point on a quadric
D = det C; -- D = 0 if and only if the six points lie on a single conic
q1 = toBracketPolynomial(D, B) --ouptut the GC expression of the determinant of the matrix C with respect to the bracketRing B
abLine = (a * b)_G -- Line joining a and b
afLine = (a * f)_G -- Line joining a and f
edLine = (e * d)_G -- Line joining e and d
efLine = (e * f)_G -- Line joining e and f
cdLine = (c * d)_G -- Line joining c and d
bcLine = (b * c)_G -- Line joining b and c
p1 = abLine ^ edLine -- Intersection point of lines joining a, b and e, d
p2 = afLine ^ cdLine -- Intersection point of lines joining a, f and c, d
p3 = bcLine ^ efLine -- Intersection point of lines joining b, c and e, f
q2 = p1 * p2 * p3 -- Span of p1, p2, p3. q = 0 if the points are collinear.
normalForm q2 === (-1) * q1 -- True! So, a,b,c,d,e,f lie on a single quadric if and only if p1, p2, p3 are collinear.
assert(net normalForm q2 == "[def]*[bcf]*[ace]*[abd]-[cef]*[bdf]*[ade]*[abc]")
///

end--

-* Development section *-
uninstallPackage "Brackets"
restart
needsPackage "Brackets"
check "Brackets"

uninstallPackage "Brackets"
restart
installPackage("Brackets", RemakeAllDocumentation => true)
needsPackage "Brackets"
viewHelp "Brackets"



restart
needsPackage "Brackets"
B = bracketRing(6,3)
r = [1 2 3]_B
s = 2 * r
end

G = gc(a..f,3)
Glu = G [l, u]

restart
needsPackage "Brackets"
B = bracketRing(8,4)

-- chow Monte Carlo
restart
needsPackage "Brackets"
B = bracketRing(4,2)
X = matrix B
u = X_{0}
v = X_{1}

chowMatrix = (u||matrix{{0},{0}}) | (matrix{{0}}||u||matrix{{0}}) | (matrix{{0},{0}}||u) | (v||matrix{{0},{0}}) | (matrix{{0}}||v||matrix{{0}}) | (matrix{{0},{0}}||v) 
chowForm = toBracketPolynomial(det chowMatrix, B)
matrix(B, flatten entries X)
det matrix(B, {{[1 2]_B,[1 3]_B,[1 4]_B},{[1 3]_B, [1 4]_B + [2 3]_B, [2 4]_B}, {[1 4]_B, [2 4]_B, [3 4]_B}})
