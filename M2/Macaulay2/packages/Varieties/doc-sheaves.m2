-- TODO: hilbertSeries and hilbertFunction used to have this in the documentation:
--"For projective varieties and coherent sheaves, the functionality is not yet implemented."
-- TODO: what would this mean?

undocumented {
    SheafExpression,
    -- TODO: document some of these
    (symbol ^,  CoherentSheaf, Array),
    (symbol _,  CoherentSheaf, Array),
    (symbol ?,  CoherentSheaf, CoherentSheaf),
    (symbol ==, CoherentSheaf, CoherentSheaf),
    (symbol ==, CoherentSheaf, ZZ),
    (symbol ==, ZZ, CoherentSheaf),
    }

document {
    Key => "coherent sheaves",
    "The main reason to implement algebraic varieties is to support the
    computation of sheaf cohomology of coherent sheaves, which does not
    have an immediate description in terms of graded modules.",
    PARA{},
    "In this example, we use ", TO "cotangentSheaf", " to produce
    the cotangent sheaf of a K3 surface and compute its sheaf
    cohomology.",
    EXAMPLE {
	"R = QQ[a,b,c,d]/(a^4+b^4+c^4+d^4);",
	"X = Proj R",
	"Omega = cotangentSheaf X",
	"hh^1(Omega)",
	"HH^1(Omega)",
	},
    PARA{},
    "We can compute the cohomology of all twists of a sheaf (by the line bundles O_X(a)
    for integers a) at once, by the command ", TO2{(hh,ZZ,SumOfTwists),"hh^i(F(*))"}, ".",
    EXAMPLE {
	"hh^1(Omega(*))",
	},
    PARA{},
    "We can also compute the cohomology of twists of a sheaf as a module over the coordinate
    ring R. (Note that it is faster to compute only the dimensions of the cohomology groups, as above.)
    Namely, the command ", TO2{(cohomology,ZZ,SumOfTwists),"HH^i(F(>=b))"}, " returns a graded R-module M
    with an R-linear map to HH^i(F(*)) that is an isomorphism in degrees at least b. (You may wish
    to apply ", TO2{truncate,"truncate(b,M)"}, " or ", TO2{prune,"prune truncate(b,M)"},
    " to the output; but that will often yield a more complicated module,
    for example with more generators.) When possible, as in the following example,
    Macaulay2 will compute the module in all degrees, even if you only asked for it in degrees at least some number.",
    EXAMPLE {
	"M2=HH^1(Omega(>=0))",
	"hilbertSeries(M2,Order=>10)",
	},
    PARA{},
    "The cohomology functions also work on closed subspaces of a weighted projective space. For example,
    let us compute a Hodge number ", TO2{(hh,Sequence,ProjectiveVariety),"hh^(p,q)(X)"}, " for the following surface
    in a weighted projective 3-space. The surface X4 is smooth as a stack, defined by an equation
    of degree 42; so its canonical line bundle is O_(X4)(42-21-14-6-1)=O_(X4). Its coarse moduli space Y has three singular points,
    which are du Val singularities of types A_1, A_2, and A_6.
    The minimal resolution of Y is a K3 surface, which has Hodge number h^(1,1) = 20, whereas X4
    has h^(1,1) equal to 20 - 1 - 2 - 6 = 11. This also computes h^(1,1)(Y), if we interpret that as h^1 of the sheaf
    of reflexive differentials on Y.",
    EXAMPLE {
	"R3=ZZ/31991[x,y,z,w,Degrees=>{21,14,6,1}];",
	"R4=R3/(x^2+y^3+z^7+w^42);",
	"X4=Proj R4;",
	"hh^(1,1)(X4)",
	},
    PARA{},
    "Use the function ", TO "sheaf", " to convert a graded module to
    a coherent sheaf, and ", TO "module", " to get the graded module
    back again.",
    EXAMPLE {
	"F = sheaf coker matrix {{a,b}}",
	"module F",
	},
    SeeAlso => {
	(cohomology, ZZ, CoherentSheaf),
	(cohomology, ZZ, SumOfTwists)
	}
    }

-----------------------------------------------------------------------------
-- Types and basic constructors and methods that return a sheaf
-----------------------------------------------------------------------------

document { Key => SheafOfRings,
    SeeAlso => { Variety, OO },
    Headline => "the class of sheaves of rings",
    EXAMPLE lines ///
      X = Proj(QQ[x..z])
      OO_X
      OO_X^6
    ///
    }

document { Key => CoherentSheaf, Headline => "the class of all coherent sheaves" }

document {
    Key => SumOfTwists,
    Headline => "the class of all sums of twists",
    "This class is used internally as an abstract representation of a graded module as an infinite direct sum of twists of a coherent sheaf.",
    EXAMPLE lines ///
	  R = QQ[x,y,z]
	  X = Proj R
	  OO_X(*)
	  peek oo
	  OO_X(>=2)
	  peek oo
	  Ext^0(OO_X^1, OO_X^1)
	  Ext^0(OO_X^1, OO_X^1(>=0))
    ///
    }

-- TODO: perhaps combine this with SumOfTwists?
document {
    Key => {
	(symbol (*), CoherentSheaf),
	(symbol (*), SheafOfRings)
	},
    Headline => "sum of twists",
    Usage => "F(*)",
    Inputs => {"F" => {" or a ", ofClass SheafOfRings}},
    Outputs => {{"a symbolic representation of the graded object consisting of the twists ", TT "F(n)", ", for all integers ", TT "n"}},
    EXAMPLE lines ///
	  R = QQ[x,y,z];
	  X = Proj R;
	  HH^0(OO_X)
	  hh^0(OO_X(*))
    ///
    }

document {
     Key => {OO, (symbol _, OO, Variety)},
     Headline => "the structure sheaf",
     Usage => "OO_X",
     Inputs => { "X" => "a variety" },
     Outputs => { { "the structure sheaf of ", TT "X", "." } },
     "Here are some computations with the sheaf of regular functions on an elliptic curve.",
     EXAMPLE lines ///
       R = QQ[x,y,z]/(y^2*z-x*(x-z)*(x-37*z));
       X = Proj R;
       OO_X
       HH^1(OO_X)
       HH^0(OO_X(3))
     ///,
     SeeAlso => {CoherentSheaf, cohomology}
     }

document { Key => sheaf, Headline => "make a coherent sheaf" }
document {
     Key => (sheaf, Variety),
     Headline => "make a coherent sheaf",
     Usage => "sheaf X",
     Inputs => {"X"},
     Outputs => {{ "the sheaf of regular functions on the variety ", TT "X" }},
     EXAMPLE lines ///
       R = QQ[x,y,z];
       X = Proj R
       Y = Spec R
       sheaf X
       sheaf Y
     ///
     }
document {
     Key => (sheaf, Variety, Ring),
     Headline => "make a coherent sheaf of rings",
     TT "sheaf(X,R)", " -- produce the coherent sheaf on the variety ", TT "X", " corresponding
     to the ring ", TT "R", ".  The variety ", TT "X", " must be ", TT "Spec R", " or ", TT "Proj R", ".",
     EXAMPLE lines ///
       R = QQ[x,y,z]
       X = Proj R
       Y = Spec R
       sheaf(X,R)
       sheaf(Y,R)
     ///}

document {
    Key => {
	(symbol ^, SheafOfRings, List),
	(symbol ^, SheafOfRings, ZZ)},
    Headline => "make a graded free coherent sheaf",
    Usage => "M = R^{i,j,k,...}",
    Inputs => {"R",
	Nothing => {TT "{i,j,k, ...}", ", ", ofClass List, ", of integers or lists of integers"}},
    Outputs => {
	CoherentSheaf => {
	    {" a graded free coherent sheaf whose generators have degrees ", TT "-i", ", ", TT "-j", ", ", TT "-k", ", ..."}}},
    EXAMPLE lines ///
	  R = QQ[a..d];
	  X = Proj R
	  OO_X^{-1,-2,3}
	  ///,
    PARA{},
    "If ", TT "i", ", ", TT "j", ", ... are lists of integers, then
    they represent multi-degrees, as in ", TO "graded and multigraded polynomial rings", ".",
    EXAMPLE lines ///
	  Y = Proj (QQ[x,y,z,Degrees=>{{1,0},{1,-1},{1,-2}}])
	  OO_Y^{{1,2},{-1,3}}
	  degrees oo
	  ///,
    SeeAlso => {OO, Proj, degrees, "graded and multigraded polynomial rings"}}

document {
    Key => {(sheaf, Variety, Module), (sheaf, Variety, Ideal)},
    Headline => "make a coherent sheaf",
    Usage => "sheaf(X,M)",
    Inputs => {"X","M"},
    Outputs => {{ "the coherent sheaf on the variety ", TT "X", " corresponding to the module ", TT "M" }},
    PARA{
	"If ", TT "X", " is the affine variety ", TT "Spec R", ", then ", TT "M", " should be an ", TT "R", "-module.  If ", TT "X", " is
	the projective variety ", TT "Proj R", ", then ", TT "M", " should be a homogeneous ", TT "R", "-module."
	}
    }

document {
    Key => {(sheaf, Module), (sheaf, Ideal), (symbol^~, Module), (symbol^~, Ideal)},
    Headline => "make a coherent sheaf",
    Usage => "sheaf M\nM^~",
    Inputs => {"M" => "homogeneous" },
    Outputs => {{ "the coherent sheaf on a projective variety X corresponding to ", TT "M" }},
    "If ", TT "M", " is an R-module and a variety X has been previously associated to R
    (for example by defining X = Proj R or X = Spec R),
    then ", TT "sheaf M", " returns the associated sheaf on X. If a variety has not been previously associated to R,
    then ", TT "M", " must be a graded module, and ", TT "sheaf M", " returns the associated sheaf on Proj R.",
    EXAMPLE lines ///
      R = QQ[x,y,z];
      X = Proj R;
      M = R^{1,2,3}
      sheaf M
      M^~
    ///
    }

document {
    Key => {(sheaf, Ring),(symbol^~, Ring)},
    Headline => "make a coherent sheaf of rings",
    Usage => "sheaf R\nR^~",
    Inputs => {"R"},
    Outputs => {{"the sheaf of regular functions on Proj ", TT "R"}},
    EXAMPLE lines ///
      R = QQ[x,y,z];
      X = Proj R;
      sheaf R
      R^~
    ///
    }

-*
doc ///
  Key
    sheaf
    (sheaf, Module)
    (sheaf, Ideal)
    (symbol ~, Module)
    (symbol ~, Ideal)
  Headline
    the coherent sheaf associated to a finitely presented graded module over a graded ring
  Usage
    sheaf M
  Inputs
    M:Module
      or @TO Ideal@, a finitely presented graded module over the ring associated to some projective variety
  Outputs
    :CoherentSheaf
      $\mathcal F$, the coherent sheaf on projective space associated to the module $M$
  Description
   Text
     Given a finitely presented graded module over a homogeneous ring $R$, one may always form a coherent
     sheaf on Proj$(R)$. This method formally constructs such an object in Macaulay2. The main difference
     between such sheaves in the affine versus projective case is that any truncation of a module will determine
     the same sheaf on Proj$(R)$. To obtain the simplest (from a readability perspective) use the @TO (prune, CoherentSheaf)@
     method.
   Text
     In the following example, we demonstrate the fact that Macaulay2 will consider different truncations
     of a module as representing the same sheaf.
   Example
     X = Proj (Q = QQ[x..z])
     F = sheaf ker vars Q
     isWellDefined F
     F' = sheaf truncate(3, module F)
     module F == module F'
     F == F'
     prune F'
   Text
     As demonstrated in the above, in order to reobtain the module representing the sheaf, use the @TO module@ command.
     Since such sheaves are determined only up to truncation, any module with finite length should determine
     the 0 sheaf:
   Example
     G = sheaf coker symmetricPower(4, vars Q)
     G == 0
   Text
     This method also works when the input is an ideal instead of a sheaf:
   Example
     m = ideal vars Q;
     H = sheaf m^4
     prune H --it "untruncates" the representing module
  Caveat
    Note that in Macaulay2, a coherent sheaf is completely determined by the data of its underlying module, and
    this data is stored rather primitively. It does not keep track of any kind of patching data on associated affine
    coverings since, computationally, such data is unnecessary.
  SeeAlso
    (sheaf, Matrix)
    (cohomology, ZZ, CoherentSheaf)
    (cohomology, ZZ, SheafMap)
///
*-

-- TODO: perhaps combine these nodes in SumOfTwists?
document {
    Key => {
	LowerBound,
	(symbol SPACE, CoherentSheaf, LowerBound),
	(symbol SPACE, SheafOfRings, LowerBound)
	},
    Headline => "the class of lower bound objects",
    "This is a type of list that represents a lower bound.  The single element of the list is an integer,
    and the object represents the condition
    that some other integer, such as the index in a direct sum, should be at least as large.",
    EXAMPLE {
	"LowerBound {4}",
	">= 4",
	"> 4"
	}}

document {
    Key => {(symbol >=, ZZ), (symbol >=, InfiniteNumber), (symbol >=, List)},
    Usage => "(>= d)",
    Inputs => { "d" },
    Outputs => {{"a special object of class ", TT "LowerBound", " used to represent the set of integers at least ", TT "d"}},
    "This can be used in a ", TO "SumOfTwists", " of a coherent sheaf, as in the following example.",
    EXAMPLE lines ///
	  R1 = ZZ/31991[x,y,z];
	  X1 = Proj R1;
	  S1 = OO_X1^1
	  HH^0(S1(>=0))
    ///,
    SeeAlso => {(symbol >,ZZ),SumOfTwists}}

document {
    Key => {(symbol >, ZZ), (symbol >, InfiniteNumber)},
    Usage => "(> d)",
    Inputs => { "d" },
    Outputs => { { "a special object of class ", TT "LowerBound", " used to represent the set of integers larger than ", TT "d" } },
    "This can be used in a ", TO "SumOfTwists", " of a coherent sheaf, as in the following example. Note that Macaulay2 may
    compute a more complete answer than you asked for.",
    EXAMPLE lines ///
	  R1 = ZZ/31991[x,y,z];
	  X1 = Proj R1;
	  S1 = OO_X1^1
	  HH^0(S1(>0))
    ///,
    SeeAlso => {(symbol >=,ZZ),SumOfTwists}}

doc ///
Node
  Key
    currentModuleMap
  Headline
    map to a simplified module representing a coherent sheaf
  Usage
    currentModuleMap F
  Inputs
    F:CoherentSheaf
  Outputs
    :Matrix
  Description
    Text
      A coherent sheaf $F$ on a projective scheme Proj $R$ is viewed in Macaulay2
      as the sheaf associated to a graded $R$-module $M$. However, any other graded $R$-module that agrees with $M$
      in sufficiently high degrees also represents the same sheaf $F$.
      The function @TT "currentModuleMap F"@ returns a map
      $M\to N$ of graded modules such that $N$ also represents $F$, but $N$ should be "simpler" than $M$
      (or at least as simple). The choice of $N$ may be improved as other functions are applied to $F$.
      This is mainly intended for internal Macaulay2 use.
    Text
      The function also works for a coherent sheaf on a closed subspace of a weighted projective space.
    Example
      R = QQ[x,y];
      X = Proj R;
      S1 = R^1/(x^2,x*y,y^2)
      currentModuleMap S1
  SeeAlso
    currentModuleBaseRing

Node
  Key
    currentModuleBaseRing
  Headline
    a simplified module representing a coherent sheaf
  Usage
    currentModuleBaseRing F
  Inputs
    F:CoherentSheaf
  Outputs
    :Module
  Description
    Text
      A coherent sheaf $F$ on a projective scheme Proj $R$ is viewed in Macaulay2
      as the sheaf associated to a graded $R$-module $M$. However, any other graded $R$-module that agrees with $M$
      in sufficiently high degrees also represents the same sheaf $F$.
      The function @TO2{currentModuleMap,"currentModuleMap F"}@ returns a map
      $M\to N$ of graded $R$-modules such that $N$ also represents $F$, but $N$ should be "simpler" than $M$
      (or at least as simple). The choice of $N$ may be improved as other functions are applied to $F$.
    Text
      The ring $R$ will be a quotient of a polynomial ring $S$,
      and @TT "currentModuleBaseRing F"@ returns $N$ as an $S$-module.
      This is mainly intended for internal Macaulay2 use.
    Text
      The function also works for a coherent sheaf on a closed subspace of a weighted projective space.
    Example
      R1 = QQ[x,y,z];
      R2 = R1/(x^2-y*z);
      X2 = Proj R2;
      S2 = OO_X2^1
      module S2
      currentModuleBaseRing S2
  SeeAlso
    currentModuleMap

Node
  Key
    (degree, CoherentSheaf)
  Headline
    the degree of a coherent sheaf on a closed subscheme of projective space
  Usage
    degree F
  Inputs
    F:CoherentSheaf
  Outputs
    :RingElement
      The output is an integer for a vector bundle on a curve $X$ in projective space,
      and a rational number when $X$ is in a more general weighted projective space.
  Description
    Text
      If @TT "F"@ is a coherent sheaf on a closed subscheme $X$ of dimension $m$ in a projective space over a field,
      then $h^0(X, F(s))$ is a polynomial of degree $m$ in $s$ for $s$ sufficiently large, known
      as the Hilbert polynomial of @TT "F"@. By definition, the leading coefficient of the Hilbert polynomial of @TT "F"@
      is $\mathrm{degree}(F)/m!$. The degree of @TT "F"@ is a positive integer (for @TT "F"@ not zero).
    Text
      More generally, for @TT "F"@ a coherent sheaf on a closed subspace $X$ of a weighted projective space,
      the Euler characteristic $\chi(X,F(s))$ is a quasipolynomial in $s$:
      $$\chi(X, F(s)) = c_m(s) s^m + \cdots + c_0(s),$$
      with each $c_j(s)$ a periodic function of $s$, of period dividing the least common multiple
      of the weights. (Equivalently, $h^0(X, F(s))$ has this description for $s$ sufficiently large.)
      Here $m$ is the dimension of the support of @TT "F"@. By definition,
      the {it average }of the leading coefficient $c_m(s)$ (with respect to $s$) is $\mathrm{degree}(F)/m!$.
      Note that the degree of a coherent sheaf in this generality is only a rational number.
      For example, for $X=\mathbf{P}^n(a_0,\ldots,a_n)$,
      the sheaf $O_X$ has degree $1/(a_0\cdots a_n)$.
    Text
      Beware that the degree of a coherent sheaf on a closed subspace of projective space
      is not directly related to the degree
      of a line bundle on a curve, @TO degreeOnCurve@. Indeed, every line bundle $L$ on a curve $X$ in $\mathbf{P}^n$ has degree
      {\it as a coherent sheaf }equal to the degree of $X$ in $\mathbf{P}^n$. The degree of $L$
      as a line bundle on $X$ can be computed as $\chi(X,L)-\chi(X,O_X)$. For a curve $X$
      in weighted projective space (viewed as a stack), the degree of $L$ on $X$ is the rational number
      @TT "lift(hilbertPolynomial L - hilbertPolynomial O_X, QQ)"@.
    Example
      R = ZZ/101[x_0..x_2];
      V = Proj R;
      S = OO_V(3);
      degree S
    Example
      R2 = ZZ/2[x,y,z,Degrees=>{1,2,3}];
      X2 = Proj R2;
      S2 = OO_X2^1;
      degree S2
  SeeAlso
    (degree,ProjectiveVariety)
    (hilbertPolynomial,CoherentSheaf)
    degreeOnCurve

Node
  Key
    (hilbertPolynomial, CoherentSheaf)
    [hilbertPolynomial, Projective]
  Headline
    the Hilbert polynomial of a coherent sheaf
  Usage
    hilbertPolynomial F
  Inputs
    F:CoherentSheaf
  Outputs
    :ProjectiveHilbertPolynomial
      If the option Projective is false, or if the coherent sheaf on a subspace of a weighted projective space
      other than the usual projective space, then the Hilbert polynomial is returned as a polynomial in @TT "QQ[i]"@.
  Description
    Text
      If @TT "F"@ is a coherent sheaf on a closed subscheme $X$ of a projective space over a field,
      then the Euler characteristics $\chi(X,F(i))$ are a polynomial function
      of the integer $i$, known as the Hilbert polynomial of $F$. (Equivalently, this is the unique polynomial in $i$
      with rational coefficients that is equal to $h^0(X,F(i))$ for $i$ sufficiently large.)
      The default is Projective => true, meaning that the output is given as a $\mathbb{Z}$-linear combination
      of the Hilbert polynomials of projective spaces of dimensions $0,\ldots,\mathrm{dim}(X)$.
    Text
      More generally, for @TT "F"@ a coherent sheaf on a closed subspace $X$
      of dimension $m$ in a weighted projective space,
      the Euler characteristic $\chi(X,F(i))$ is a quasipolynomial in $i$:
      $$\chi(X, F(i)) = c_m(i) i^m + ... + c_0(i),$$
      with each $c_j(i)$ a periodic function of $i$, of period dividing the least common multiple
      of the weights. (Equivalently, $h^0(X, F(i))$ has this description for $i$ sufficiently large.)
      Note that the twist $F(i)$ is defined by tensoring with the line bundle $O(i)$ on $X$ as a stack.
    Text
      We define the Hilbert polynomial $f(i)$ of $F$ as the polynomial in @TT "QQ[i]"@ obtained by
      {\it averaging} each of the coefficients $c_j(i)$. In particular, with this definition,
      the leading coefficient of the Hilbert polynomial is $\mathrm{degree}(F)/m!$.
      Note that the degree of a coherent sheaf on a closed subspace
      in a weighted projective space is only a rational number. For example,
      for $X=\mathbf{P}^n(a_0,\ldots,a_n)$, the sheaf $O_X$ has degree $1/(a_0\cdots a_n)$.
    Text
      We compute the Hilbert polynomial for the ideal sheaf of a cubic curve on the projective plane,
      and for the structure sheaf of a weighted projective plane:
    Example
      R = ZZ/101[x_0..x_2];
      V = Proj R;
      S = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})
      h = hilbertPolynomial S
      hilbertPolynomial(S, Projective=>false)
    Example
      R2 = ZZ/2[x,y,z,Degrees=>{1,2,3}];
      X2 = Proj R2;
      S2 = OO_X2^1
      hilbertPolynomial S2
  SeeAlso
    (degree,CoherentSheaf)
    (hilbertPolynomial,ProjectiveVariety)
    degreeOnCurve

Node
  Key
    canonicalBundle
   (canonicalBundle, ProjectiveVariety)
   [canonicalBundle, MinimalGenerators]
   [canonicalBundle, Strategy]
  Headline
    the canonical bundle of a projective variety
  Usage
    canonicalBundle X
  Inputs
    X:ProjectiveVariety
    MinimalGenerators => Boolean
      whether to @TO prune@ the result before returning it. The default value is true.
  Outputs
    :CoherentSheaf
  Description
    Text
      This function computes the canonical bundle $K_X$ of a projective scheme $X$ over a base ring $k$.
      This is the line bundle
      $\Omega^n_X$ of $n$-forms if $X$ is smooth of dimension $n$ over $k$. In general, it is a coherent sheaf, defined as the
      reflexive hull (that is, the double dual) of the sheaf $\Omega^n_X$, where $n$ is the rank of $\Omega^1_X$.
    Text
      The function also works for closed subspaces of a weighted projective space.
    Text
      The following example shows some cases of Serre duality and some computations of the geometric genus.
    Example
      R1 = QQ[x_0..x_3];
      X = Proj R1 -- That is, P^3.
      omega = canonicalBundle X
      for i to 3 list hh^i (tangentSheaf X)(-1)
      for i to 3 list hh^i (dual( (tangentSheaf X)(-1)) ** omega)
      --I = ideal(x_1*x_2-x_0*x_3,x_2^3-x_1*x_3^2,x_0*x_2^2-x_1^2*x_3,x_1^3-x_0^2*x_2
      --ideal(x_2^2-x_1*x_3,x_1*x_2-x_0*x_3,x_1^2-x_0*x_2)
      Y = Proj(R1/(x_0^5+x_1^5+x_2^5+x_3^5))
      isSmooth Y
      omega' = canonicalBundle Y
      C = cotangentSheaf(2,Y);
      for i to 2 list hh^i (C)
      for i to 2 list hh^i ((dual C) ** omega') --dual
    Text
      We can use this to see the difference between the top exterior power of the cotangent bundle of Z and its reflexive hull.
    Example
      Z = Proj(R1/(x_0^2*x_1 - x_2^2*x_3))
      isSmooth Z
      for i to 2 list hh^i canonicalBundle Z
      for i to 2 list hh^i det cotangentSheaf Z
    Text
      The geometric genus of a smooth projective variety is defined as the dimension of the vector space
      of global sections of the canonical bundle.
      Projective space has geometric genus 0, and elliptic curves have genus 1. We verify the elliptic curve
      case in an example:
    Example
      R2 = QQ[x..z]/(y^3 - y*z^2 - x^3)
      E = Proj R2
      for i to 1 list hh^i canonicalBundle E
    Text
      For a hypersurface $X$ defined by an equation of degree $d$ in a weighted projective space
      $\mathbf{P}^n(a_0,\ldots,a_n)$, the canonical bundle is $K_X=O(d-a_0-\cdots-a_n)$.
    Example
      R3 = ZZ/31991[x,y,z,w,Degrees=>{1,1,2,3}];
      R4 = R3/(x^6+y^6+z^3+w^2);
      X4 = Proj R4;
      canonicalBundle X4
  Caveat
      The function does not check that the input variety $X$ is normal,
      but rather always returns the reflexive hull of the top exterior power of the cotangent sheaf.
      Also, the function only gives reasonable output for $X$ equidimensional.
  SeeAlso
    cotangentSheaf

Node
  Key
   (canonicalBundle, AffineVariety)
  Headline
    canonical bundle of an affine variety
  Usage
    canonicalBundle X
  Inputs
    X:AffineVariety
    MinimalGenerators => Boolean
      whether to @TO prune@ the result before returning it
  Outputs
    :CoherentSheaf
  Description
    Text
      This method computes the canonical bundle $K_X$ of an affine scheme $X$ over a base ring $k$. This is the line bundle
      $\Omega^n_X$ of $n$-forms if $X$ is smooth of dimension $n$ over $k$. In general, it is a coherent sheaf, defined as the
      reflexive hull (that is, the double dual) of the sheaf $\Omega^n_X$, where where $n$ is the rank of $\Omega^1_X$.
    Text
      We compute the canonical bundle of some affine surfaces.
    Example
      R0 = QQ[x,y,z];
      R1 = R0/(x^2+y^2+z^2-1);
      X1 = Spec R1;
      S1 = canonicalBundle X1
      isLocallyFree S1
      use R0;
      R2 = R0/(x^2+y^2+z^2);
      X2 = Spec R2;
      S2 = canonicalBundle X2
      isLocallyFree S2
  Caveat
      The function does not check that the input variety $X$ is normal,
      but rather always returns the reflexive hull of the top exterior power of the cotangent sheaf.
      Also, the function only gives reasonable output for $X$ equidimensional.
  SeeAlso
    idealSheaf
    cotangentSheaf
    reflexiveDifferentials
    (cotangentSheaf, ZZ, ProjectiveVariety)
    AffineVariety
Node
  Key
    tangentSheaf
   (tangentSheaf, ProjectiveVariety)
   [tangentSheaf, MinimalGenerators]
   [tangentSheaf, Strategy]
  Headline
    tangent sheaf of a projective variety
  Usage
    tangentSheaf X
  Inputs
    X:ProjectiveVariety
    MinimalGenerators => Boolean
      whether to @TO prune@ the result before returning it. The default value is true.
  Outputs
    :CoherentSheaf
  Description
    Text
      This method computes the tangent sheaf $TX$ of a projective variety $X$. It is defined as
      the dual of the @TO cotangentSheaf@. If $X$ is smooth, then $TX$ is a vector bundle.
    Text
      The function also works for a closed subspace $X$ of a weighted projective space. Here $X$ is viewed
      as a stack. If $X$ is smooth as a stack, then $TX$ is a vector bundle on $X$.
    Text
      Tangent bundle of the projective plane:
    Example
      P2 = Proj QQ[a,b,c];
      TP = tangentSheaf P2
      hh^0(TP(-1))
      hh^1(TP(-3))
    Text
      The tangent sheaf of the nodal cubic curve and the cuspidal cubic curve:
    Example
      N = Proj QQ[a,b,c]/(b^2*c-a^2*(a+c));
      TN = tangentSheaf N
      hh^0(TN), hh^1(TN)
      C = Proj QQ[a,b,c]/(b^2*c-a^3);
      TC = tangentSheaf C
      hh^0(TC), hh^1(TC)
    Text
      This calculation shows that the nodal cubic has automorphism group of dimension 1, while the cuspidal cubic
      has automorphism group of dimension 2. Also, the vanishing of $H^1(X,TX)$ for each of these curves $X$ says that
      every locally trivial first-order deformation of $X$ is trivial.
      --- TODO: update this once we can compute the kernel
      --- of HH^0(phi) for a sheaf map phi: F -> G
  SeeAlso
    idealSheaf
    cotangentSheaf
    ProjectiveVariety

Node
  Key
    cotangentSheaf
   (cotangentSheaf, ProjectiveVariety)
   [cotangentSheaf, MinimalGenerators]
   [cotangentSheaf, Strategy]
  Headline
    cotangent sheaf of a projective variety
  Usage
    cotangentSheaf X
  Inputs
    X:ProjectiveVariety
    MinimalGenerators => Boolean
      whether to @TO prune@ the result before returning it
  Outputs
    :CoherentSheaf
  Description
    Text
      This function computes the cotangent sheaf of a closed subscheme of a projective space
      over a base ring.
    Text
      More generally, @TT "cotangentSheaf X"@ works for $X$ a closed subspace of a weighted projective space,
      viewed as a stack. For example, if the coarse moduli space of $X$ is quasi-smooth and well-formed,
      then the stack $X$ is smooth, and
      the pushforward of the cotangent sheaf to the coarse moduli space is the sheaf of reflexive differentials.
      If $X$ is normal but not quasi-smooth, the sheaf of @TO reflexiveDifferentials@ is often more useful.
    Text
      If the characteristic $p$ is positive and $p$ divides some of the weights, the weighted projective space
      (and its substacks)
      need not be Deligne-Mumford stacks. In that situation, this function returns the cohomology sheaf
      in degree 0 of the cotangent complex of $X$.
      In that situation, it may be more natural to consider the truncation of the cotangent complex
      to cohomological degrees $\geq 0$ (which lives in degrees 0 and 1), given by @TO{"naiveCotangentComplex"," X"}@.
    Text
      As an example, we verify the Gauss-Bonnet theorem on a plane quintic curve. That is, the degree
      of the canonical bundle of a smooth projective curve of genus $g$ is $2g-2$.
    Example
      X = Proj QQ[x,y,z]/(x^5+y^5+z^5)
      genus X
      omega = cotangentSheaf X
      degreeOnCurve omega
  SeeAlso
    idealSheaf
    tangentSheaf
    cotangentSurjection
    (cotangentSheaf, ZZ, ProjectiveVariety)
    ProjectiveVariety

Node
  Key
    idealSheaf
   (idealSheaf, ProjectiveVariety)
   (idealSheaf, AffineVariety)
   [idealSheaf, MinimalGenerators]
   [idealSheaf, Strategy]
  Headline
    ideal sheaf of a variety
  Usage
    idealSheaf X
  Inputs
    X:{AffineVariety,ProjectiveVariety}
    MinimalGenerators => Boolean
      whether to @TO prune@ the result before returning it. Default value is true.
  Outputs
    :CoherentSheaf
  Description
    Text
      This method computes the ideal sheaf of the variety $X$ in its
      @TO2{(ambient,AffineVariety),"ambient variety"}@, typically affine space
      or projective space.
    Text
      As an example, we compute the ideal sheaf of a quartic curve in $\mathbf{P}^2$.
    Example
      R1 = QQ[x,y,z];
      X = Proj R1/(x^4+y^4+z^4);
      I = idealSheaf X
      rho = inducedMap((ambient I)/I, ambient I) --induced inclusion of ideal sheaf into structure sheaf of ambient ring
      for i to 2 list hh^i(image rho)
  SeeAlso
    cotangentSheaf
    ProjectiveVariety

Node
  Key
    (cotangentSheaf, ZZ, ProjectiveVariety)
  Headline
    exterior powers of the cotangent sheaf of a projective variety
  Usage
    cotangentSheaf(m, X)
  Inputs
    m:ZZ
    X:ProjectiveVariety
    MinimalGenerators => Boolean
      whether to @TO prune@ the result before returning it
  Outputs
    :CoherentSheaf
  Description
    Text
      This function computes the $m$th exterior power of the @TO2 {cotangentSheaf, "cotangent sheaf"}@
      of a closed subscheme $X$ of projective space over a field, usually denoted $\Omega_X^m$
      or the sheaf of $m$-forms.
    Text
      More generally, cotangentSheaf(m, X) works for $X$ a closed subspace of a weighted projective space,
      viewed as a stack.
      For example, if the coarse moduli space of $X$ is quasi-smooth and well-formed,
      then the pushforward of this sheaf to the coarse moduli space is the sheaf of reflexive $m$-forms.
      If $X$ is normal but not quasi-smooth, use the command @TO {reflexiveDifferentials, "(m, X)"}@ instead,
      if you want the sheaf of reflexive $m$-forms.
    Text
      If the characteristic $p$ is positive and $p$ divides some of the weights, the weighted projective space
      (and its substacks)
      need not be Deligne-Mumford stacks. In that situation, it may be more natural to consider
      @TO {naiveCotangentComplex,"(m, X)"}@.
    Text
      As an example, we compute $h^{1,1}$ of a K3 surface (a quartic in projective 3-space):
    Example
      K3 = Proj ZZ/31991[x_0..x_3]/(x_0^4+x_1^4+x_2^4+x_3^4-11*x_0*x_1*x_2*x_3)
      omega1 = cotangentSheaf(1, K3);
      hh^1(omega1)
    Text
      As a second example we compute @TO2 {(hh, Sequence, ProjectiveVariety), "Hodge numbers"}@
      of the Fermat quintic 3-fold:
    Example
      FermatQuintic = Proj QQ[x_0..x_4]/(x_0^5+x_1^5+x_2^5+x_3^5+x_4^5)
      omega1 = cotangentSheaf(1, FermatQuintic);
      hh^1(omega1)
      hh^2(omega1)
      omega2 = cotangentSheaf(2, FermatQuintic);
      hh^1(omega2)
      hh^2(omega2)
  SeeAlso
    cotangentSheaf
    ProjectiveVariety
    (hh, Sequence, ProjectiveVariety)

-----------------------------------------------------------------------------
-- Basic methods for sheaves
-----------------------------------------------------------------------------

Node
  Key
    (variety, CoherentSheaf)
    (variety, SheafOfRings)
    (variety, SumOfTwists)
    (variety, SheafMap)
    variety
  Headline
    the underlying variety over which a coherent sheaf or morphism is defined.
  Usage
    variety F
  Inputs
    F:{CoherentSheaf,SheafOfRings,SumOfTwists,SheafMap}
  Outputs
    :Variety
  Description
    Text
      For Macaulay2, a coherent sheaf @TT "F"@ is defined on a particular scheme $X$
      (or subspace of weighted projective space), even if the support of @TT "F"@ is smaller than $X$.
      This matters in some situations; for example, computing global
      @TO2{(Ext,ZZ,CoherentSheaf,CoherentSheaf),"Ext^i(F,G)"}@ between two sheaves
      requires them to be defined on the same scheme (or stack) $X$. As a result,
      one may need to move sheaves around by @TO2{directImage,
      "directImage(F,Y)"}@ or @TO2{(pullback,CoherentSheaf,Variety),"pullback(F,X)"}@.
    Example
      X = Proj QQ[x,y,z];
      variety OO_X
      variety OO_X(3)
      variety id_(OO_X(3))

Node
  Key
    (ring, CoherentSheaf)
    (ring, SheafOfRings)
    (ring, SumOfTwists)
    (ring, SheafMap)
  Headline
    the coordinate ring of the underlying variety
///

-- TODO: combine these two
document {
    Key => (module, CoherentSheaf),
    Headline => "get the module defining a coherent sheaf",
    Usage => "module F",
    Inputs => {"F"},
    Outputs => {{"the module from which the coherent sheaf ", TT "F", " was defined"}},
    EXAMPLE lines ///
      R = QQ[x,y,z];
      X = Proj R;
      F = OO_X(3)
      module F
      degrees oo
    ///,
    SeeAlso => { OO, degrees, Proj }
    }
document {
    Key => (module, SheafOfRings),
    SeeAlso => { Variety, OO },
    Usage => "module F",
    Inputs => { "F" },
    Outputs => { { "the module corresponding to ", TT "F" }},
    EXAMPLE lines ///
      R = QQ[x..z];
      X = Proj R;
      OO_X
      module oo
    ///
    }
-- TODO: should (module, SumOfTwists) return HH^0(F) or module F?

doc ///
Node
  Key
    (isWellDefined, CoherentSheaf)
  Headline
    whether a coherent sheaf is well-defined
  Usage
    isWellDefined F
  Inputs
    F:CoherentSheaf
  Outputs
    :Boolean
--  Description
--    Text
--    Example
  SeeAlso
    (isWellDefined, Variety)
--    (isWellDefined, Module)

Node
  Key
   (isLocallyFree, Module)
  Headline
    whether a module is locally free
  Usage
    isLocallyFree M
  Inputs
    M:Module
  Outputs
    :Boolean
  Description
    Text
      This function determines whether a module $M$ over $R$ is locally free.
      Since $M$ is finitely generated and $R$ is noetherian, it is equivalent to check
      whether $M$ is projective, or whether $M$ is flat.
    Text
      The ring $R$ need not be a domain. As a result, a locally free module may have different ranks
      on different components of $\mathrm{Spec}(R)$.
    Example
      R1 = QQ[x,y];
      R2 = R1/(x^3 + y^3 - 1);
      M3 = R2^1;
      isLocallyFree M3
      M4 = M3/(x-1,y);
      isLocallyFree M4
      f4 = inducedMap(M4, M3);
      M5 = kernel f4;
      isLocallyFree M5
    Text
      The module M5 is locally free but not free.
    Text
      Internally, the algorithm uses a computation of @TO2 {fittingIdeal, "Fitting ideals"}@.
  SeeAlso
    (isLocallyFree, CoherentSheaf)
    (rank, Module)
    fittingIdeal
    isFreeModule

Node
  Key
    isLocallyFree
   (isLocallyFree, SumOfTwists)
   (isLocallyFree, SheafOfRings)
   (isLocallyFree, CoherentSheaf)
  Headline
    whether a coherent sheaf is locally free
  Usage
    isLocallyFree F
  Inputs
    F:{SumOfTwists,SheafOfRings,CoherentSheaf}
  Outputs
    :Boolean
  Description
    Text
      This function determines whether a coherent sheaf $\mathcal F$ on $X$ is locally free.
      Local freeness means that $X$ can be covered by affine open subschemes $U$ for which
      $\mathcal F(U)$ is a free $\mathcal O_X(U)$-module. In other words, $F$ is a vector bundle.
    Text
      Here $X$ may be affine, or else contained in a projective or weighted projective space
      over a field. It need not be integral. As a result, a locally free sheaf may have different ranks
      on different components of $X$.
    Example
      X = Proj QQ[x,y,z];
      isLocallyFree OO_X
      isLocallyFree OO_X^{1,2,3}
      F = cotangentSheaf X
      isLocallyFree F
    Text
      Internally, the algorithm uses a computation of @TO2 {fittingIdeal, "Fitting ideals"}@.
  SeeAlso
    (isLocallyFree, Module)
    (rank, CoherentSheaf)
    fittingIdeal
    isFreeModule
///

document {
    Key => (dim, CoherentSheaf),
    Headline => "dimension of the support of a coherent sheaf",
    Usage => "dim F",
    Inputs => {"F"
	},
    Outputs => {ZZ
	},
    "Computes the dimension of the support of a coherent sheaf ", TT "F", ". The answer given
    is definitely valid if ", TT "F", " is defined on an affine variety or on Proj(R) for a singly graded ring R.",
    PARA{},
    "For a ring R graded by an abelian group of rank m, this definition views Proj(R)
    as the quotient of an (unspecified) open subset of Spec(R) by a generically stable action
    of a torus of dimension m. So the function returns the dimension of F as an R-module minus m,
    or rather the maximum of -1 with that number. That is not the standard definition of Proj; but it may
    be useful if developed further. For now, many commands for projective varieties
    are restricted to the singly graded case.",
    EXAMPLE {
	"R = ZZ/101[x_0..x_4];",
	"A = matrix{{x_0,x_1,x_2,x_3},{x_1,x_2,x_3,x_4}}",
	"M = R^1/minors(2,A);",
	"S = sheaf M",
	"dim S",
	"dim M"
	},
    SeeAlso => {Proj, (dim, AffineVariety), (dim, ProjectiveVariety)}
    }

document {
    Key => {(codim, CoherentSheaf), [(codim, CoherentSheaf), Generic]},
    Headline => "codimension of the support of a coherent sheaf on a variety",
    Usage => "codim F",
    Inputs => {"F" => {"a coherent sheaf over a ", TO "Variety", TT " X"}},
    Outputs => {ZZ},
    "Computes the codimension of the support of ", TT "F", ". If the sheaf ", TT "F", " is zero,
    we return infinity. Otherwise, we interpret the codimension as ", TT "dim(R) - dim(M)",
    ", where ", TT "M", " is the module representing ", TT "F", " over the homogeneous coordinate ring ",
    TT "R", " of ", TT "X", ".",
    PARA{},
    TT "Generic => true", " allows the computation of the codimension to proceed without an error message,
    even if the ring is defined over the integers. In effect, the computation proceeds
    by tensoring first with the rational numbers.",
    EXAMPLE {
	  "R = ZZ/31991[a,b,c,d];",
          "I = monomialCurveIdeal(R,{1,3,5})",
          "projplane = Proj(R)",
          "II = sheaf module I",
          "can = sheafExt^1(II,OO_projplane^1(-4))",
          "codim can"
	  },
    Caveat => {"The returned value is the usual codimension if ", TT "R",
	" is an integral domain or, more generally, equidimensional."},
    SeeAlso => {(dim,Module),(codim,Variety)}
    }

doc ///
Node
  Key
    (rank, CoherentSheaf)
  Headline
    rank of a coherent sheaf
  Usage
    rank F
  Inputs
    F:CoherentSheaf
  Outputs
    :ZZ
  Description
    Text
      For a coherent sheaf @TT "F"@ on an integral scheme $X$, this function returns the rank of @TT "F"@, that is,
      the dimension of the generic fiber of @TT "F"@ as a vector space over the function field
      of $X$. (Note that a coherent sheaf @TT "F"@ in Macaulay2 is defined on a specific space $X$.
      If @TT "F"@ is supported on a lower-dimensional subset of $X$, its rank is 0.)
    Text
      For $X$ not integral, the output of this function may not be reasonable.
      At least it gives the expected value for arbitrary $X$ if @TT "F"@ is a vector bundle
      of constant rank on $X$.
    Example
      R1 = ZZ/31991[x,y,z];
      X1 = Proj R1;
      S1 = cotangentSheaf X1;
      rank S1
      M1 = R1^1/(x^2-y*z);
      S2 = sheaf M1;
      rank S2
  SeeAlso
    (isLocallyFree, CoherentSheaf)
    (rank, Module)
    (degree, Module)
    (degree, Ring)

Node
  Key
    (degrees, CoherentSheaf)
  Headline
    degrees of a coherent sheaf
  Usage
    degrees F
  Inputs
    F:CoherentSheaf
  Outputs
    :List
  Description
    Text
      A coherent sheaf @TT "F"@ on a projective scheme $X = \mathrm{Proj}(R)$
      is represented in Macaulay2 as the sheaf associated
      to a graded $R$-module $M$. Also, $M$ is represented with a given set of generators.
      This function returns the degrees of those generators. This is not actually an invariant of @TT "F"@.
  SeeAlso
    (degrees, Module)
///

document {
    Key => (numgens, CoherentSheaf),
    Headline => "the number of generators of the underlying module",
    Usage => "numgens F",
    Inputs => {"F"},
    Outputs => { ZZ => {"number of generators of the underlying module ", TT "M", " of ", TT "F"} },
    "In Macaulay2, each coherent sheaf is defined as the sheaf associated to a certain module over
    the coordinate ring, and that module has a given set of generators. This number of generators
    is not an invariant of ", TT "F", ".",
    EXAMPLE {
	"R = QQ[a..d]/(a^3+b^3+c^3+d^3);",
	"X = Proj R;",
	"T' = cotangentSheaf X",
	"numgens T'",
	"module T'"
	},
    SeeAlso => {(module, CoherentSheaf), tangentSheaf}
    }

-* TODO: does this still make sense? compare with (betti, Module)
Node
  Key
   (betti, CoherentSheaf)
  Heading
    Betti diagram showing the degrees of generators and relations of a homogeneous module or coherent sheaf
  Usage
    betti M
  Inputs
    M:{Module,CoherentSheaf}
    Weights=>List
  Outputs
    :BettiTally
      showing the zero-th, first graded, and total Betti numbers of $M$.
  Description
    Text
      Note that the Betti numbers are not minimized.
    Example
      S = ZZ/10007[x,y];
      betti coker matrix{{x^3, x*y^2}, {y*x^2, y^3}}
      betti coker map(S^{0,-1}, , matrix{{x^2, y}, {y^3, x^2}})
*-

-- TODO: (super, CoherentSheaf)
-- TODO: (cover, CoherentSheaf)
-- TODO: (ambient, CoherentSheaf)

document {
    Key => (pdim, CoherentSheaf),
    Headline => "calculate the projective dimension",
    Usage => "pdim F",
    Inputs => {"F" => CoherentSheaf},
    Outputs => {ZZ => "the projective dimension"},
    "In Macaulay2, a coherent sheaf is given as the sheaf associated to a certain module M over the coordinate ring R.
    This function returns the projective dimension of M as an R-module, that is, the minimum length
    of a projective resolution of M over R.",
    EXAMPLE {
	"V = Proj(ZZ/101[x_0..x_2]);",
	"F = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})",
	"pdim F"}
    }

doc ///
Node
  Key
    (minimalPresentation, CoherentSheaf)
    (prune,               CoherentSheaf)
    (minimalPresentation, SheafMap)
    (prune,               SheafMap)
  Headline
    minimal presentation of a coherent sheaf or sheaf map
  Usage
    prune F
  Inputs
    F:CoherentSheaf
  Outputs
    :CoherentSheaf
      a sheaf represented by the depth 2-ification of the original representative module
  Description
    Text
      For a coherent sheaf on an affine scheme, this function yields an isomorphic sheaf, with an attempt
      to simplify the presentation of the corresponding module.
    Text
      For a coherent sheaf on a projective scheme over a base ring, this function yields an isomorphic sheaf,
      but here the underlying module may have changed.
      Given a sheaf $\mathcal F$ represented by some module $M$, there are always isomorphisms
      $$H^i(X, \mathcal{F}) = H^i_{\mathfrak{m}}(M)_0 \quad \text{for} \ i > 0,$$
      where $\mathfrak{m}$ denotes the irrelevant ideal. When $i = 0$ the best one can say in general
      is that there is an exact sequence
      $$0 \to H^0_{\mathfrak{m}}(M) \to \bigoplus_{d \geq 0} H^0(X, \mathfrak{F}(d)) \to M \to H^1_{\mathfrak{m}}(M) \to 0.$$
      Thus, the prune command for a coherent sheaf computes a graded module $M'$ representing the same
      sheaf $\mathcal{F}$, but such that there is an honest isomorphism
      $$M' \cong \bigoplus_{d \geq 0} H^0(X, \mathcal{F} (d)).$$
      In other words, the prune command computes a module with depth at least 2 that represents the same sheaf $\mathcal{F}$.
    Example
      R = QQ[x,y];
      M1 = module ideal(x^2,x*y,y^2)
      S1 = sheaf M1
      S2 = prune S1
    Text
      The pruning isomorphism of sheaves is cached with the key {\tt pruningMap}. It is an isomorphism of sheaves
      coming from a map of modules that need not be an isomorphism.
    Example
      S2.cache.pruningMap
    Text
      Pruning also applies in a functorial way to morphisms of sheaves.
    Text
      The method for computing the pruned representative proceeds with a few steps: first, the representative module
      $M$ is replaced by $M / H^0_{\mathfrak{m}} (M)$, which kills any torsion. Next, since $M$ is now torsion-free, we have
      an equality $\operatorname{Hom}_S (N , M) = 0$ for any module $N$ with finite length. Thus, we can choose
      a large formal Frobenius power $\mathfrak{m}^{[m]} = (x_1^m , \dots , x_n^m)$ of the maximal ideal so that $\operatorname{Hom}_S (\mathfrak{m}^{[m]} , M)$
      has depth at least $2$. Applying the functor $\operatorname{Hom}_S ( - , M)$ to the short exact sequence
      $$0 \to \mathfrak{m}^{[m]} \to S \to S / \mathfrak{m}^{[m]} \to 0$$
      yields a short exact sequence
      $$0 \to \operatorname{Hom}_S (\mathfrak{m}^{[m]} , M) \to M \to \operatorname{Ext}^1_S (S / \mathfrak{m}^{[m]} , M) \to 0.$$
      The induced map $\operatorname{Hom}_S (\mathfrak{m}^{[m]} , M) \to M$ is not an isomorphism of modules,
      but yields an isomorphism on sheaves since the cokernel has finite length. This induced sheaf map is the pruning map,
      and the new representative is the sheaf associated to the module $\operatorname{Hom}_R (\mathfrak{m}^{[m]} , M)$.
  Caveat
    Since the pruning operation for sheaves tends to be much more involved than for modules,
    pruning sheaves at every step of a computation may cause significant slowdowns.
  SeeAlso
    prune
///
-* TODO: this is a piece of the documentation of (prune, Module)
-- (minimalPresentation, CoherentSheaf), (prune, CoherentSheaf)
     "This function also works when ", TT "M", " is ", ofClass{GradedModule,ChainComplex,CoherentSheaf}, ",
     by acting on the modules and maps within it.",
     EXAMPLE lines ///
	  I = ideal(a^2,b^3,c^4,d^7)
     	  X = Proj R
	  J = I^~
	  minimalPresentation J
     ///,
*-

-----------------------------------------------------------------------------
-- Arithmetic operations
-----------------------------------------------------------------------------

document {
    Key => {(symbol SPACE, CoherentSheaf, ZZ), (symbol SPACE, SheafOfRings, ZZ)},
    Headline => "twist of a coherent sheaf",
    Usage => "F(a)",
    Inputs => {"F" => {"or ", ofClass SheafOfRings, ", on a projective variety"}, "a"},
    Outputs => { CoherentSheaf => "the twist of F on a projective scheme X by the a-th power
	of the hyperplane line bundle O(1)" },
    "This also works for a coherent sheaf ", TT "F", " on a closed subspace X of a weighted projective space. In that setting,
    O(1) is a line bundle on X as a stack.",
    EXAMPLE lines ///
      R = ZZ/31991[x,y,z];
      X = Proj R;
      F = OO_X;
      G = F(3)
      module G
      degrees oo
    ///,
    SeeAlso => {(symbol SPACE,Module,ZZ),(symbol SPACE,Ring,ZZ)}
    }
-- TODO: (symbol SPACE, CoherentSheaf, Sequence)
-- TODO: also for SumOfTwists?

document {
    Key => {(symbol SPACE, Module, ZZ), (symbol SPACE, Module, Sequence)},
    Headline => "twist of a graded module",
    Usage => "M(a)", -- "M(a_1,...,a_r)",
    Inputs => {"M", "a"},
    Outputs => { Module => "the twist of M by the integer a,
        or by the sequence of integers (a_1,...,a_r)" },
    "For ", TT "M", " a graded module over a graded ring R, ", TT "M(a)", " is the same module but shifted down in degree
    by ", TT "a", ". This also works for a multigraded ring, with the notation ", TT "M(a_1,...,a_r)", ".",
    EXAMPLE lines ///
      R = QQ[x,y,z];
      M = R^1;
      M(3)
    ///,
    SeeAlso => {(symbol SPACE,CoherentSheaf,ZZ),(symbol SPACE,Ring,ZZ)}
    }
document {
    Key => {(symbol SPACE, Ring, ZZ), (symbol SPACE, Ring, Sequence)},
    Headline => "twist of a graded ring",
    Usage => "R(a)", -- "R(a_1,...,a_r)"
    Inputs => {"R", "a"},
    Outputs => { Module => "the twist of R by the integer a,
	or by the sequence of integers (a_1,...,a_r)" },
    "For a graded ring ", TT "R", ", ", TT "R(a)", " is the free ", TT "R", "-module of rank 1,
    but shifted down in degree by ", TT "a", ". Another notation for the same thing is ", TT "R^{a}", ". This also works
    for a multigraded ring, with the notation ", TT "R(a_1,...,a_r)", ".",
    EXAMPLE lines ///
      R = QQ[x,y,z];
      R(3)
    ///,
    SeeAlso => {(symbol SPACE,CoherentSheaf,ZZ),(symbol SPACE,Module,ZZ)}
    }

document {
    Key => (dual, CoherentSheaf),
    Headline => "dual coherent sheaf",
    Usage => "dual F",
    Inputs => {"F" => CoherentSheaf},
    Outputs => {CoherentSheaf},
    "This function returns the dual of a coherent sheaf. That is, for ", TT "F", " defined on a space X,
    return the sheaf of O_X-linear maps from ", TT "F", " to O_X."
    }

-- TODO: should this move up, next to SheafOfRings ^ List?
document {
    Key => {
	(symbol ^, CoherentSheaf, ZZ),
	(symbol ^, CoherentSheaf, List)},
    Headline => "direct sum",
    Usage => "F^n",
    Inputs => {"F" => {", or ", ofClass SheafOfRings}, "n"},
    Outputs => {CoherentSheaf => {"the direct sum of ", TT "n", " copies of ", TT "F"},},
    EXAMPLE lines ///
      R = QQ[a..d]/(a*d-b*c);
      Q = Proj R;
      OO_Q^5
      IL = sheaf module ideal(a,b)
      IL^3
    ///,
    SeeAlso => {Proj, sheaf}
    }

-- TODO: (directSum, CoherentSheaf)?
--this should be updated to mention the caching of components that now occurs
document {
    Key => (symbol ++, CoherentSheaf, CoherentSheaf),
    Headline => "direct sum of coherent sheaves",
    Usage => "F ++ G",
    Inputs => {"F","G"},
    Outputs => {{"the direct sum of ", TT "F", " and ", TT "G"}},
    EXAMPLE lines ///
      X = Proj(QQ[x,y,z])
      OO_X(3) ++ OO_X(4)
      module oo
    ///
    }

document {
    Key => (symbol **, CoherentSheaf, CoherentSheaf),
    Headline => "tensor product of coherent sheaves",
    Usage => "F ** G",
    Inputs => {"F","G"},
    Outputs => {{"the tensor product of ", TT "F", " and ", TT "G"}},
    EXAMPLE lines ///
      X = Proj(QQ[x,y,z])
      OO_X(-3) ++ OO_X(4)
      oo ** oo
    ///
    }

document {
    Key => (symbol ^**, CoherentSheaf, ZZ),
    Headline => "tensor power",
    Usage => "M^**i",
    Inputs => {"M" , "i" },
    Outputs => {CoherentSheaf => { "the ", TT "i", "-th tensor power of ", TT "M"}},
    "The second tensor power of the canonical bundle of a smooth
    rational quartic curve in P^3:",
    EXAMPLE lines ///
      R = QQ[a..d];
      I = monomialCurveIdeal(R,{1,3,4})
      X = variety I
      KX = sheaf(Ext^1(I,R^{-4}) ** ring X)
      K2 = KX^**2
      prune K2
    ///,
    "Notice that the resulting sheaf is not always presented in the most
    economical manner.  Use ", TO prune, " to improve the presentation.",
    SeeAlso => {monomialCurveIdeal, Ext, variety, sheaf, prune}
    }

document {
    Key => {(symbol /, CoherentSheaf, CoherentSheaf), (symbol /, CoherentSheaf, Ideal)},
    Headline => "quotient of coherent sheaves",
    Usage => "F / G",
    Inputs => { "F", "G" => {"or ", ofClass Ideal} },
    Outputs => { CoherentSheaf => {"the quotient sheaf ", TT "F/G"} },
    "We compute the cohomology of two sheaves supported on a plane cubic curve C in X = P^2,
    the trivial bundle O_C and the conormal bundle N_(C/X) (which has degree -9 on C).",
    EXAMPLE lines ///
      X = Proj(QQ[x,y,z]);
      I = ideal(y^2*z-x*(x-z)*(x-11*z));
      N = (sheaf module I)/(sheaf module I^2)
      G = OO_X^1/I
      hh^1(G)
      hh^1(N)
    ///,
    SeeAlso => {Proj, Spec, sheaf, (cohomology,ZZ,CoherentSheaf), OO}
    }

-- TODO: (symbol *, Ideal, CoherentSheaf)

document {
    Key => (exteriorPower, ZZ, CoherentSheaf),
    Usage => "exteriorPower(i,F)",
    Inputs => {"i","F"},
    Outputs => {{ "the ", TT "i", "-th exterior power of ", TT "F"}}
    }

document {
    Key => (symmetricPower, ZZ, CoherentSheaf),
    Usage => "symmetricPower(i,F)",
    Inputs => {"i","F"},
    Outputs => {{ "the ", TT "i", "-th symmetric power of ", TT "F"}}
    }

document {
    Key => (determinant, CoherentSheaf),
    Usage => "determinant F",
    Inputs => {"F"},
    Outputs => {{ "the determinant of ", TT "F"}},
    "That is, if ", TT "F", " has ", TO2 {(rank,CoherentSheaf),"rank"}, " r over a space X, the function returns the r-th exterior power
    of ", TT "F", " over X, which will have rank 1. If ", TT "F", " is not a vector bundle, you might prefer to take
    the reflexive hull (double ", TO "dual", ") of this sheaf. In another direction,
    you may wish to ", TO2 {(minimalPresentation,CoherentSheaf),"prune"}, " this sheaf, to simplify later calculations.",
    PARA{},
    "Note that the rank of ", TT "F", " (and hence this function)
    is only guaranteed to behave well for X integral. Macaulay2 does give
    the expected answer for the rank on arbitrary X if ", TT "F", " is a vector bundle of constant rank."
    }


--Should the output of this be of type CoherentSheaf instead?
doc ///
Node
  Key
   (annihilator, CoherentSheaf)
  Headline
    the annihilator ideal
  Usage
    ann F
    annihilator F
  Inputs
    F:{CoherentSheaf}
    Strategy=>Symbol
      either @TT "Quotient"@ or @TT "Intersection"@; see @TO [annihilator, Strategy]@
  Outputs
    :Ideal
      the annihilator ideal
  Description
    Text
      @stacksProject("0H2G", "The annihilator of a coherent sheaf")@ $\mathcal F$
      is the ideal corresponding to the kernel of the map of sheaves
      $$ \mathcal O_X \to \mathcal Hom_{\mathcal O_X}(\mathcal F, \mathcal F). $$

      You may use @TT "ann"@ as a synonym for @TT "annihilator"@.
  SeeAlso
    (annihilator, Module)
    (isSupportedInZeroLocus, Ideal, Module)
///
