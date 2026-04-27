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
    "We may use ", TO "Spec", " to create an affine scheme (or algebraic variety) with
    a specified coordinate ring and ", TO "ring", " to recover the ring.",
    EXAMPLE {
	"R = ZZ/2[x,y,z]",
	"X = Spec R",
	"ring X",
	"dim X",
	},
    "The variety ", TT "X", " is a 3-dimensional affine space.",
    PARA{},
    "We may form products.",
    EXAMPLE {
	"X ** X",
	"dim oo",
	},
    PARA{},
    "We may use ", TO "Proj", " to create a projective scheme (or algebraic variety)
    with a specified homogeneous coordinate ring.",
    EXAMPLE {
	"Y = Proj R",
	"ring Y",
	"dim Y",
	},
    "The most important reason for introducing the notion of algebraic variety into a computer
    algebra system is to support the notion of coherent sheaf.  See ", TO "coherent sheaves", "
    for information about that.",
    PARA{},
    "For more details about varieties, see ", TO "Variety", "."
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
    EXAMPLE lines ///
    R = QQ[x,y];
    Proj R
    ///
    }

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

document { Key => (ring, Variety), Headline => "coordinate ring of the variety" }

document {
    Key => (ideal, Variety),
    Headline => "returns the defining ideal",
    Usage => "ideal X",
    Inputs => {"X"},
    Outputs => { Ideal => {"which is the defining ideal of ", TT "X"} },
    "A ", TO2("Variety", "variety"), " is defined by a ", TO2("Ring", "ring"),
    ".  This function returns the defining ideal of the ring of ", TT "X", ".",
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
      whether $X$ is a well-defined affine or projective variety
--  Description
--    Text
--    Example
///

-- TODO: [(codim, Variety), Generic]
document {
    Key => (codim, Variety),
    Headline => "codimension of a variety",
    Usage => "codim V",
    Inputs => {"V"},
    Outputs => {ZZ},
    "Computes the codimension of a variety ", TT "V", ".",
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
    Headline => "dimension of the affine variety",
    Usage => "dim V",
    Inputs => {"V"},
    Outputs => {ZZ},
    "Computes the dimension of the affine algebraic set ", TT "V"," as the Krull dimension
    of its affine coordinate ring.",
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
    Headline => "dimension of the projective variety",
    Usage => "dim V",
    Inputs => {"V"
	},
    Outputs => {ZZ
	},
    "Computes the dimension of the projective algebraic set from
    the Krull dimension of its homogeneous coordinate ring.",
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
     "Returns the characteristic of the corresponding ring."
     }

doc ///
Node
  Key
    (ambient,     AffineVariety)
    (ambient, ProjectiveVariety)
  Headline
    the ambient affine or projective space of an embedded variety
  Usage
    ambient X
  Inputs
    X:{AffineVariety,ProjectiveVariety}
  Outputs
    :{AffineVariety,ProjectiveVariety}
      which is either an affine space or projective space where $X$ lies
--  Description
--    Text
--    Example
  SeeAlso
    (ambient, Ring)

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
      The singular locus of a variety is the set of singular points of some variety $X$. Geometrically
      it is the intersection of the points in the variety $X$ with the points at which the associated
      jacobian matrix does not have maximal rank. Algebraically, this corresponds to the variety
      defined the ideal obtained by taking the sum of the defining ideal of $X$ and the maximal minors
      of the Jacobian matrix. In the projective case, one should also saturate to make sure the ideal
      defines a reduced object.
    Example
      R = QQ[x,y]/(x^3 - y^2);
      X = Spec R
      S = singularLocus X
      codim S
      trim ideal ring S
    Text
      In the projective case, there may be additional irrelevant components that we would like to saturate
      away:
    Example
      Q = QQ[x,y]/(x^2*y);
      Y = Proj(Q)
      S' = singularLocus Y
      I = ideal ring S'
      J = minors(1,jacobian ideal Q) + ideal Q
      primaryDecomposition J
    Text
      As we see in the above, without saturating there is an irrelevant component arising
      from the ideal $(x^2 , y)$ that disappears upon saturating. 
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
      S = QQ[x,y,z]
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
   (isSmooth, Variety)
  Headline
    whether a variety is smooth
  Usage
    isSmooth X
  Inputs
    X:Variety
  Outputs
    :Boolean
  Description
    Text
      An affine variety $X$ is smooth if the @TO2 (singularLocus, "singular locus")@ of $X$ is empty,
      while a projective variety is smooth if the singular locus is supported at the origin.
    Example
      R = quotient minors_2 genericMatrix(QQ[x,y,z,w], 2, 2)
      isSmooth Proj R
      isSmooth Spec R
  SeeAlso
    "NormalToricVarieties::isSmooth(NormalToricVariety)"
///

document {
    Key => (degree, ProjectiveVariety),
    Usage => "degree X",
    Inputs => { "X" },
    Outputs => { ZZ => {"the degree of ", TT "X"} },
    EXAMPLE {
	"S = ZZ/32003[x,y,z];",
	"I = ideal(x^4-4*x*y*z^2-z^4-y^4);",
	"R = S/I;",
	"X = variety I",
	"degree X"
	},
    "The degree of a projective variety ", TT "X = V(I) = Proj R", " is the degree
    of the homogeneous coordinate ring ", TT "R = S/I", " of ", TT "X", ".",
    EXAMPLE {
	"degree X == degree I",
	"degree X == degree R"
	},
    SeeAlso => {(degree,Ideal),variety, "varieties"}
    }

document {
    Key => (hilbertPolynomial, ProjectiveVariety),
    Headline => "compute the Hilbert polynomial of the projective variety",
    Usage => "hilbertPolynomial V",
    Inputs => { "V" => ProjectiveVariety },
    Outputs => { ProjectiveHilbertPolynomial => "unless the option Projective is false" },
    "We compute an example of the ",
    TO2(hilbertPolynomial, "Hilbert polynomial"), " of a projective
    Hilbert variety. This is the same as the Hilbert polynomial of
    its coordinate ring.",
    EXAMPLE {
	"R = QQ[a..d];",
	"I = monomialCurveIdeal(R, {1,3,4});",
	"V = Proj(R/I)",
	"h = hilbertPolynomial V",
	"hilbertPolynomial(V, Projective=>false)"
	},
    PARA{},
    "These Hilbert polynomials can serve as ",
    TO2 (hilbertFunction,"Hilbert functions"),
    " too since the values of the Hilbert polynomial eventually are
    the same as the Hilbert function of the sheaf of rings or of the underlying ring.",
    EXAMPLE {
	"apply(5, k-> h(k))",
	"apply(5, k-> hilbertFunction(k,ring V))"
	}
    }

-----------------------------------------------------------------------------
-- Arithmetic operations
-----------------------------------------------------------------------------

doc ///
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
