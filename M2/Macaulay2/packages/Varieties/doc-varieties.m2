undocumented {
    (describe, ProjectiveVariety),
    (describe, AffineVariety),
    (symbol /,         AffineVariety, Thing),
    (symbol /,     ProjectiveVariety, Thing),
    (symbol SPACE,     AffineVariety, Array),
    (symbol SPACE, ProjectiveVariety, Array),
    }

-----------------------------------------------------------------------------
-- Types and basic constructors and methods that return a variety
-----------------------------------------------------------------------------

document { Key => Variety, Headline => "the class of all algebraic varieties", SeeAlso => "varieties" }
document { Key => AffineVariety, Headline => "the class of all affine varieties" }
document { Key => ProjectiveVariety, Headline => "the class of all projective varieties" }

document { Key => "varieties",
    "This package can analyze affine or projective varieties, including coherent sheaves on them
    and sheaf cohomology. More generally, it can work with closed subspaces of weighted projective spaces.",
    PARA{},
    "We may use ", TO "Spec", " to create an affine scheme (or algebraic variety) with
    a specified coordinate ring and ", TO "ring", " to recover the ring.",
    EXAMPLE {
	"R = ZZ/2[x0,x1,x2];",
	"X = Spec R",
	"ring X",
	"dim X",
	},
    "The variety ", TT "X", " is a 3-dimensional affine space A^3 over the field of order 2.",
    PARA{},
    "We may use ", TO "Proj", " to create a projective scheme (or algebraic variety)
    with a specified homogeneous coordinate ring. An example is the projective plane P^2:",
    EXAMPLE {
	"Y = Proj R",
	"ring Y",
	"dim Y",
	},
    PARA{},
    "The ring R may have generators in different degrees,
    in which case ", TT "Proj R", " is a closed subspace of a weighted projective space. (When the distinction matters,
    a weighted projective space and its subspaces are viewed as algebraic stacks.)",
    PARA{},
    "The coarse moduli space of a weighted projective space is a projective variety; so it could be embedded
    in a higher-dimensional projective space. But it is more efficient that Macaulay2 can compute directly
    with weighted projective spaces.",
    EXAMPLE {
	"R1 = QQ[w,x,y,Degrees=>{1,2,3}];",
	"X1 = Proj R1",
	"isSmooth X1",
	"R2 = R1/(y^2 - x^3 - 3*x*w^4 - 5*w^6);",
	"X2 = Proj R2",
	"genus X2",
	},
    PARA{},
    "Macaulay2 views the weighted projective plane X1 as an algebraic stack; as such, it is smooth over Q,
    although its coarse moduli space has two singular points (the points [0,1,0] and [0,0,1] with nontrivial
    stabilizer groups, of order 2 or 3). Concretely, X1 is the quotient of A^3 - 0 by the multiplicative group
    G_m, acting by t(w,x,y) = (tw, t^2 x, t^3 y).",
    PARA{},
    "Next, X2 is an elliptic curve; in fact,
    the Weierstrass equation of an elliptic curve is naturally viewed as embedding the curve into
    the weighted projective plane X1 = P^2(1,2,3), as shown here.",
    PARA{},
    "The key reason for introducing the notion of algebraic variety into a computer
    algebra system is to support the notion of coherent sheaves and their cohomology.  See ", TO "coherent sheaves", ".",
    PARA{},
    "For more details about varieties, see ", TO "Variety", ".",
    }

document {
    Key => {Spec, (Spec, Ring)},
    Headline => "make an affine variety",
    Usage => "Spec R",
    Inputs => {"R"},
    Outputs => {{ "the affine variety (or scheme) formed from the ring ", TT "R" }},
    EXAMPLE lines ///
    R = QQ[x,y];
    Spec R
    ///
    }

document {
    Key => {Proj, (Proj, Ring)},
    Headline => "make a projective variety",
    Usage => "Proj R",
    Inputs => {"R"},
    Outputs => {{ "the projective variety (or scheme) formed from the graded ring ", TT "R" }},
    "When R is a graded ring with generators in degree 1, ", TT "Proj R", " is a closed subscheme
    of a projective space over the base ring.",
    PARA{},
    "When R is a graded ring with generators in different (positive) degrees, X = ", TT "Proj R", " is
    a closed subspace of a weighted projective space. More precisely, it is viewed in Macaulay2 as the quotient stack
    [(Y-0)/G_m], where Y = Spec(R) and G_m is the multiplicative group, acting with the given weights.
    For many purposes, such as computing sheaf cohomology, it is harmless to identify X with the
    coarse moduli space of this stack.",
    PARA{},
    "For example, we can define the projective line over the rational numbers.",
    EXAMPLE lines ///
    R = QQ[x,y];
    Proj R
    ///
    }

doc ///
Node
  Key
    ProjectiveSpace
    ProjectiveStack
  Headline
    construct a projective space or weighted projective stack
  Usage
    ProjectiveSpace^n
    ProjectiveStack(a,b,c)
    ProjectiveSpace_kk^n
    ProjectiveStack_kk(a,b,c)
  Inputs
    n:ZZ
    "(a,b,c)":Sequence
    kk:Ring
  Outputs
    :ProjectiveVariety
      projective space or weighted projective stack over the ring @TT "kk"@
  Description
    Example
      PP = ProjectiveSpace
      PP^2
      PP_(ZZ/101)^1
      PP(1,1,2)
      PP_(ZZ/101)(1,1,2)
  SeeAlso
    "NormalToricVarieties :: toricProjectiveSpace"
    "NormalToricVarieties :: weightedProjectiveSpace"
///

-- document { Key => variety, Headline => "get the variety" }
-- for (variety, CoherentSheaf), etc. see doc-sheaves.m2
document {
    Key => (variety, Ideal),
    Headline => "the closed projective subvariety defined by an ideal",
    Usage => "variety I",
    Inputs => { "I" => "a homogeneous ideal" },
    Outputs => {"the closed subvariety defined by an ideal"},
    Caveat => {
	"An alternative task for this function would be to define the affine subvariety,
	so if something like this eventually becomes useful,
	we may have to redesign it.  Suggestions welcome." },
    "In the example, we compute the dimension of a line in the projective plane.",
    EXAMPLE lines ///
    R = QQ[x..z]
    variety ideal x
    dim oo
    ///
    }

doc ///
  Key
    (variety, Ring)
  Headline
    the variety previously associated to a given ring
  Usage
    variety S
  Inputs
    S:Ring
      the intersection ring of a variety $X$, say, or the homogeneous (Cox) ring of a normal toric variety,
      or another ring that has been associated to a variety, or an element in such a ring
  Outputs
    :Variety
      $X$, the variety associated with {\tt S}
  Description
   Text
     If a variety has not previously been associated with the ring {\tt S}, this function returns @TO2{"Proj","Proj S"}@.
     If you want the affine scheme @TO2{"Spec","Spec S"}@, use that command.
   Example
     needsPackage "NormalToricVarieties"
     X = toricProjectiveSpace 1
     S = ring X
     X === variety S
   Example
     needsPackage "Schubert2"
     Y = abstractProjectiveSpace 1
     IY = intersectionRing Y
     Y === variety IY
   Text
     For package developers: All this function does is to look up the symbol {\tt variety} in {\tt S}.
     This is currently used in two packages, but can be used in other settings, if desired.
  SourceCode
     (variety, Ring)
  SeeAlso
    "NormalToricVarieties::NormalToricVarieties"
    "Schubert2::Schubert2"
///

-----------------------------------------------------------------------------
-- Basic methods for varieties
-----------------------------------------------------------------------------

document {
    Key => (ring, Variety),
    Headline => "coordinate ring of a variety",
    Usage => "ring X",
    Inputs => {"X"},
    Outputs => { Ring => {"which is the coordinate ring of ", TT "X"} },
    "A ", TO2("Variety", "variety"), " is defined in Macaulay2 as Spec(R) (for a commutative ring R)
    or as Proj(R) (for a commutative graded ring R). This function returns the ring R.",
    SeeAlso => {(ideal,Variety), Spec, AffineVariety, Proj, ProjectiveVariety}
    }

document {
    Key => (ideal, Variety),
    Headline => "returns the defining ideal",
    Usage => "ideal X",
    Inputs => {"X"},
    Outputs => { Ideal => {"which is the defining ideal of ", TT "X"} },
    "A ", TO2("Variety", "variety"), " is defined by a ", TO2("Ring", "ring"),
    " R.  When R was defined as a quotient ring S/I, this function returns the ideal I.
    Otherwise, for example if R is a polynomial ring, this function returns
    the zero ideal.",
    EXAMPLE {
	"R = QQ[w,x,y,z];",
	"X = Spec(R/(y^2-x*z,x^2*y-z^2,x^3-y*z))",
	"ideal X",
	"ring X",
	"Y = Proj(R/(x^2-w*y, x*y-w*z, x*z-y^2))",
	"ideal Y"
	},
    SeeAlso => {ring, (ideal,Ring), Spec, AffineVariety, Proj, ProjectiveVariety}
    }

doc ///
Node
  Key
    (isWellDefined, Variety)
  Headline
    whether a variety is well-defined
  Usage
    isWellDefined X
  Inputs
    X:Variety
  Outputs
    :Boolean
      whether @TT "X"@ is a well-defined affine or projective variety
  Description
    Text
      The function checks whether @TT "X"@ has the data structure it should. Also, if @TT "X"@
      was defined as Proj(R), it checks that the ring R is singly graded, with generators
      in positive degrees.
///

document {
    Key => {(codim, Variety), [(codim, Variety), Generic]},
    Headline => "codimension of a variety",
    Usage => "codim V",
    Inputs => {"V"},
    Outputs => {ZZ},
    "Computes the codimension of a variety ", TT "V", " in the ambient affine space or projective space.",
    PARA{},
    TT "Generic => true", " allows the computation of the codimension to proceed without an error message,
    even if the ring is defined over the integers. In effect, the computation proceeds
    by tensoring first with the rational numbers.",
    EXAMPLE {
	"R = ZZ/101[x_0..x_3];",
	"M = matrix{{x_0,x_1,x_2},{x_1,x_2,x_3}}",
	"V = Proj(R/minors(2,M));",
	"codim V"
	},
    Caveat => {"The returned value is the usual codimension if the base ring
	is an integral domain or, more generally, equidimensional."},
    SeeAlso => {(codim,QuotientRing)}
    }


document {
    Key => (dim, AffineVariety),
    Headline => "dimension of an affine variety",
    Usage => "dim V",
    Inputs => {"V"},
    Outputs => {ZZ},
    "Computes the dimension of the affine algebraic set ", TT "V",". Equivalently, this is the Krull dimension
    of its coordinate ring.",
    EXAMPLE {
	"R = ZZ/101[x,y];",
	"point = ideal(x,y);",
	"line = ideal(2*x+3*y-1);",
	"V=Spec(R/intersect(point,line))",
	"dim V",
	"Z=Spec(R/(point+line))",
	"dim Z"
	},
    SeeAlso => {Spec, (dim, ProjectiveVariety)}
    }
document {
    Key => (dim, ProjectiveVariety),
    Headline => "dimension of a projective variety",
    Usage => "dim V",
    Inputs => {"V"
	},
    Outputs => {ZZ
	},
    "Computes the dimension of a projective scheme (or closed subspace of a weighted projective space) ", TT "V",
    ". The answer given is definitely valid if V is Proj(R) for a singly graded ring R.",
    PARA{},
    "For a ring graded by an abelian group of rank m, this definition views Proj(R)
    as the quotient of an (unspecified) open subset of Spec(R) by a generically stable action
    of a torus of dimension m. So the function returns max(dim(R) - m, -1).
    That is not the standard definition of Proj; but it may
    be useful if developed further. For now, many commands for projective varieties
    are restricted to the singly graded case.",
    EXAMPLE {
	"R = ZZ/101[x_0..x_4];",
	"M = matrix{{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4}}",
	"V = Proj(R/minors(2,M));",
	"degree V",
	"dim V",
	"dim minors(2,M)"
	},
    SeeAlso => {Proj, (dim, AffineVariety)}
    }

-- TODO: document the difference between the affine and projective cases
document {
     Key => {(char, AffineVariety), (char, ProjectiveVariety)},
     Headline => "the characteristic of the coordinate ring of a variety",
     "Returns the characteristic of the corresponding ring R. That is, it is zero if the ring of integers Z
     injects into R, and otherwise it is the smallest positive integer that is equal to zero in R."
     }

doc ///
Node
  Key
    (ambient,     AffineVariety)
    (ambient, ProjectiveVariety)
  Headline
    the space containing a given variety, often an affine or projective space
  Usage
    ambient X
  Inputs
    X:{AffineVariety,ProjectiveVariety}
  Outputs
    :{AffineVariety,ProjectiveVariety}
      which is an affine or projective variety containing $X$.
  Description
    Text
      In most cases,
      this will be affine or projective space (or a weighted projective space). But if $X$ was defined as a subspace
      of some other subspace $Y$ in affine or projective space, $Y$ will be returned.
      One can obtain the ambient projective space (for example) as: @TT "Proj ring presentation ring X"@.
  SeeAlso
    (ambient, Ring)
    directImage
    (pullback, CoherentSheaf, Variety)

Node
  Key
    (singularLocus,     AffineVariety)
    (singularLocus, ProjectiveVariety)
  Headline
    the locus of singular points in a variety
  Usage
    singularLocus X
  Inputs
    X:{AffineVariety,ProjectiveVariety}
  Outputs
    :{AffineVariety,ProjectiveVariety}
  Description
    Text
      The singular locus of a variety $X$ is the subset of $X$ at which the associated
      jacobian matrix does not have maximal rank. Algebraically, the defining ideal of the singular locus
      is the sum of the defining ideal of $X$ and the maximal minors
      of the Jacobian matrix. In the projective case, this command saturates that ideal, to get a simpler ideal
      that defines the same singular locus in projective space.
    Example
      R = QQ[x,y]/(x^3 - y^2);
      X = Spec R
      S = singularLocus X
      codim S
      trim ideal ring S
    Text
      In the projective case, @TT "singularLocus"@ simplifies the Jacobian ideal by saturating,
      as the following calculation shows.
    Example
      Q = QQ[x,y]/(x^2*y);
      Y = Proj(Q)
      S' = singularLocus Y
      I = ideal ring S'
      J = minors(1,jacobian ideal Q) + ideal Q
      primaryDecomposition J
    Text
      As we see in the above, without saturating there is an irrelevant component arising
      from the ideal $(x^2,y)$ that disappears upon saturating.
  SeeAlso
    (singularLocus, Ring)
///


-----------------------------------------------------------------------------
-- Methods specific to projective varieties
-----------------------------------------------------------------------------
-- for euler, genus, and genera see their respective documentation pages

doc ///
Node
  Key
    isProjective
   (isProjective, Variety)
   (isProjective, ProjectiveVariety)
  Headline
    whether a variety is projective
  Usage
    isProjective X
  Inputs
    X:Variety
  Outputs
    :Boolean
  Description
    Text
      This method essentially returns whether $X$ was defined using @TO Proj@ or @TO Spec@.
    Example
      S = QQ[x,y,z];
      isProjective Proj S
      isProjective Spec S
  Caveat
    This method does absolutely no well-definedness checks and is very primitive. To check well-definedness
    use @TO isWellDefined@ instead.
  SeeAlso
    "NormalToricVarieties::isProjective(NormalToricVariety)"
///


--Comment about this code: isn't it sufficient to just check that the quotient by the
--jacobian + the original ideal has finite length? Using the singularLocus command to do this
--adds an additional saturation computation that seems unnecessary
doc ///
Node
  Key
   (isSmooth, AffineVariety)
   (isSmooth, ProjectiveVariety)
  Headline
    whether a variety is smooth
  Usage
    isSmooth X
  Inputs
    X:{AffineVariety,ProjectiveVariety}
  Outputs
    :Boolean
  Description
    Text
      An variety $X$ is smooth if its @TO2 (singularLocus, "singular locus")@ is empty.
      When $X$ is a closed subspace of a weighted projective space, this function checks
      whether $X$ is smooth as a stack over the base ring.
    Example
      R = quotient minors_2 genericMatrix(QQ[x,y,z,w], 2, 2)
      isSmooth Proj R
      isSmooth Spec R
  SeeAlso
    "NormalToricVarieties::isSmooth(NormalToricVariety)"

Node
  Key
    (degree, ProjectiveVariety)
  Headline
    the degree of a projective variety
  Usage
    degree X
  Inputs
    X:ProjectiveVariety
  Outputs
    :RingElement
      The output is an integer for a closed subscheme $X$ of projective space,
      and a rational number when $X$ is in a more general weighted projective space.
  Description
    Text
      If @TT "X"@ is a closed subscheme of dimension $m$ in a projective space over a field,
      its degree can be defined as the intersection number $\int_X c_1(O_X(1))^m$.
      This is a positive integer (for @TT "X"@ not empty).
    Text
      More generally, for a closed subspace @TT "X"@ of a weighted projective space
      of dimension $m$,
      its degree can again be defined as the intersection number $\int_X c_1(O_X(1))^m$.
      This is a positive rational number (for @TT "X"@ not empty). This is compatible
      with the definition of the @TO2 {hilbertPolynomial,"Hilbert polynomial"}@ of @TT "X"@.
      For example, $X=\mathbf{P}^n(a_0,\ldots,a_n)$ has degree $1/(a_0\cdots a_n)$.
    Text
      We compute the degree (in this sense) for a curve of degree 4 in $\mathbf{P}^2$,
      and for a curve defined by an equation of degree 5 in a weighted projective plane.
    Example
      S = ZZ/101[x,y,z];
      I = ideal(x^4-4*x*y*z^2-z^4-y^4);
      R = S/I;
      X = Proj R
      degree X
    Example
      S2 = QQ[u,v,w,Degrees=>{1,2,3}];
      I2 = ideal(u^5+v*w);
      R2 = S2/I2;
      X2 = Proj R2
      degree X2
  SeeAlso
    (degree,CoherentSheaf)
    (hilbertPolynomial,ProjectiveVariety)
    degreeOnCurve

Node
  Key
    (hilbertPolynomial, ProjectiveVariety)
  Headline
    the Hilbert polynomial of a projective variety
  Usage
    hilbertPolynomial X
  Inputs
    X:ProjectiveVariety
  Outputs
    :ProjectiveHilbertPolynomial
      If the option Projective is false, or if @TT "X"@ is given as a subspace of a weighted projective space
      other than the usual projective space, then the Hilbert polynomial is returned as a polynomial in @TT "QQ[i]"@.
  Description
    Text
      If @TT "X"@ is a closed subscheme $X$ of a projective space over a field,
      then the Euler characteristics $\chi(X,O(i))$ are a polynomial function
      of the integer $i$, known as the Hilbert polynomial of $X$. (Equivalently, this is the unique polynomial in $i$
      with rational coefficients that is equal to $h^0(X,O(i))$ for $i$ sufficiently large.)
      The default is Projective => true, meaning that the output is given as a $\mathbb{Z}$-linear combination
      of the Hilbert polynomials of projective spaces of dimensions $0,\ldots,\mathrm{dim}(X)$.
    Text
      More generally, for @TT "X"@ a closed subspace
      of dimension $m$ in a weighted projective space,
      the Euler characteristic $\chi(X,O(i))$ is a quasipolynomial in $i$:
      $$\chi(X, O(i)) = c_m(i) i^m + ... + c_0(i),$$
      with each $c_j(i)$ a periodic function of $i$, of period dividing the least common multiple
      of the weights. (Equivalently, $h^0(X, O(i))$ has this description for $i$ sufficiently large.)
      Note that $O(i)$ is a line bundle on the stack $X$, for each integer $i$.
    Text
      We define the Hilbert polynomial $f(i)$ of $X$ as the polynomial in @TT "QQ[i]"@ obtained by
      {\it averaging} each of the coefficients $c_j(i)$. In particular, with this definition,
      the leading coefficient of the Hilbert polynomial is $\mathrm{degree}(X)/m!$.
      Note that the degree of a closed subspace
      of a weighted projective space is only a rational number. For example,
      $\mathbf{P}^n(a_0,\ldots,a_n)$ has degree $1/(a_0\cdots a_n)$.
    Text
      We compute the Hilbert polynomial for a smooth rational curve of degree 4 in projective 3-space,
      the projective closure of the affine curve $(t,t^3,t^4)$:
    Example
      R = QQ[a..d];
      I = monomialCurveIdeal(R, {1,3,4})
      V = Proj(R/I);
      h = hilbertPolynomial V
      hilbertPolynomial(V, Projective=>false)
    Text
      We compute the Hilbert polynomial for the ideal sheaf of a cubic curve on the projective plane,
      and for the structure sheaf of a weighted projective plane:
    Example
      R = ZZ/101[x_0..x_2];
      V = Proj R;
      S = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})
      h = hilbertPolynomial S
      hilbertPolynomial(S, Projective=>false)
    Text
      For a subspace of the usual projective space, the values of the Hilbert polynomial
      are eventually the same as the @TO2{hilbertFunction,"Hilbert function"}@.
    Example
      apply(5, k-> h(k))
      apply(5, k-> hilbertFunction(k,ring V))
    Example
      R2 = ZZ/2[x,y,z,Degrees=>{1,2,3}];
      X2 = Proj R2;
      hilbertPolynomial X2
  SeeAlso
    (degree,ProjectiveVariety)
    (hilbertPolynomial,CoherentSheaf)
    degreeOnCurve

-----------------------------------------------------------------------------
-- Arithmetic operations
-----------------------------------------------------------------------------

Node
  Key
    (symbol **, AffineVariety, AffineVariety)
    (symbol **, AffineVariety, Ring)
  Headline
    Cartesian product of two affine varieties
  Synopsis
    Usage
      X ** Y
    Inputs
      X:AffineVariety
      Y:AffineVariety
    Outputs
      :AffineVariety
	the Cartesian product $X \times Y$
    Description
      Example
	X = Spec QQ[x,y]
	Y = X ** X
	describe Y
  Synopsis
    Usage
      X ** R
    Inputs
      X:AffineVariety
      R:Ring
    Outputs
      :AffineVariety
	the Cartesian product $X \times \mathrm{Spec} R$
    Description
      Example
	Z = X ** QQ[t]
	describe Z
  Caveat
    Cartesian products of non-affine varieties are not yet implemented in this package,
    but see @TO "NormalToricVarieties::cartesianProduct(NormalToricVariety)"@ for another implementation.
///
