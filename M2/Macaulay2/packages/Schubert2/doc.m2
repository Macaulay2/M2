Node
  Key
    Schubert2
  Headline
    computation in intersection theory
  Description
    Text

      This package supports computation in intersection theory on smooth projective varieties.  An @TO2{AbstractVariety,"abstract
      variety"}@ is not given by equations.  Instead, one gives its graded intersection ring of algebraic cycle classes
      modulo numerical equivalence (tensored with the rational numbers or perhaps
      with some algebra over the rational numbers), its dimension, a method for counting the number of points in a cycle class
      of dimension zero (integration), and the Chern class of its tangent bundle (if
      known).  The intersection ring is represented as a {\em Macaulay2} @ TO2{Ring,"ring"} @, and its elements are {\em
      Macaulay2} @ TO2{RingElement,"ring elements"} @.
      
      An @TO2{AbstractSheaf,"abstract sheaf"}@ on an abstract variety is represented by an object of class 
      @ TO AbstractSheaf @ that contains the total Chern character of the sheaf.  It should be thought of as an element of
      the Grothendieck ring of the variety tensored with the rational numbers, which, by the
      Grothendieck-Riemann-Roch theorem, is isomorphic to the intersection ring; the isomorphism is provided
      by the Chern character.

      An @TO2{AbstractVarietyMap,"abstract variety map"}@ is a map between abstract varieties: the information encoded
      is the pull-back and push-forward maps between the corresponding intersection rings.  If necessary, the push-forward map
      is computed automatically from multiplication and integrals.

      One may start with a {\em base variety} whose intersection ring contains variables of degree 0 that can represent
      unknown integers in formulas, along with variables of positive degree that can be used as Chern classes of
      generic abstract sheaves.

      This package is inspired by @ HREF{"http://stromme.uib.no/home/schubert/", "schubert"} @, which was written by
      @ HREF{"http://www.math.uiuc.edu/~katz/", "Sheldon Katz"} @ and @ HREF{"http://stromme.uib.no/", "Stein A. Strømme"} @.

      The theory behind the computations of intersection rings of flag bundles has been made rigorous in the paper 
      {\em Computations in intersection rings of flag bundles} by Grayson, Seceleanu, and Stillman:
      see @ HREF {"http://arxiv.org/abs/1205.4190","arXiv:1205.4190"} @.  The paper also treats the case
      of isotropic flag bundles, with the corresponding algorithms appearing here for the first time.

  Subnodes
    "Lines on hypersurfaces"
    "Conics on a quintic threefold"
    "The Horrocks-Mumford bundle"
    "Riemann-Roch on a curve"
    "Riemann-Roch on a surface"
    "Riemann-Roch without denominators"
    "A cubic fourfold containing a degree-5 del Pezzo surface"    
    "Examples from Schubert, translated"
    "Ideas for future development"
    :methods not involving something exported by this package:
    (symbol _, OO, RingElement)
    (symbol SPACE,OO,RingElement)
--------
Node
 Key
  "Ideas for future development"
 Description
  Text
   @
    UL {
	 LI {"Introduce the notation F/S for the structure map from F to S, and extend it to towers of structure 
	      maps.  Then we could downplay ", TO StructureMap, ".  We could also change ", TT "tangentBundle F", "
	      so it yields the relative tangent bundle over the immediate predecessor, and let ", 
	      TT "tangentBundle(F/point)", " serve as the notation for the absolute tangent bundle 
	      (see ", TO "tangentBundle", ").  We could also adopt the notation ", TT "OO_(F/S)(n)", " for
	      tautological sheaves."
	      },
	 LI {"compare logg and expp to the routines in Schubert, and try to speed them up"},
	 LI {
	      "Maybe we should distinguish between cohomology and homology."
	      },
	 LI {"eliminate the generators from the intersection ring of a flag bundle that correspond to
	      the Chern classes of the first tautological subquotient bundle"},
	 LI {"change the default names ", TT "H_(i,j)", " for the Chern classes of the tautological subquotient bundles
	      on a flag variety to ", TT "chern_j E_i"},
	 LI {"make an easy way to specify maps from a variety to a flag bundle, by specifying where all (but one)
	      of the tautological subquotients go"},
	 LI {"make it possible to call the toric package"},
	 LI {"add the blow up of a subvariety with known normal bundle and known restriction function"},
	 LI {"add a function that goes from the Hilbert series of a sheaf on projective space to the Chern class"},
	 LI {"add knowledge of the intersection ring of an abelian variety, in particular of the Jacobian of a curve"},
	 LI {"add knowledge of the tautological ring (as far as it's known!) of M_(g,n)"},
	 LI {"add excess intersection formula"},
	 LI {"add double (and multiple) point formula"},
	 LI {"add Bott's formula, useful for counting twisted cubics, as described in ", TT "Schubert2/Stromme/" }
	 }
   @
-------
Node
  Key
     "Lines on hypersurfaces"
  Headline
     an example
  Description
   Text
	There are d+1 conditions for a line to be
	contained in a general hypersurface of degree d in $\PP^n$.
	The Grassmannian of lines in $\PP^n$ has dimension 2(n-1).  Therefore,
	when d+1 = 2(n-1), we should expect a finite number of lines.
	Here is a way of computing the number using {\em Schubert2}.  In the case
	of lines on a quintic hypersurface in $\PP^4$, this computation was done
	by Hermann Schubert in 1879.

	We will first illustrate the method by computing the number
	of lines on a cubic surface in $\PP^3$.

	We first construct @ofClass AbstractVariety@ representing
	the Grassmannian of lines in $\PP^3$ and then retrieve its tautological sub-
	and quotient bundles.
   Example
     G = flagBundle({2,2}, VariableNames => {,c})
     (S,Q) = bundles G
   Text
	Any cubic surface is given by a cubic form on $\PP^3$,
	that is, an element of the third symmetric power of the
	space of linear forms, which is the trivial rank 4 bundle
	on $\PP^3$.  Its image in the third symmetric power $Symm^3 Q$  of the
	quotient bundle $Q$ vanishes at those points of the
	Grassmannian that correspond to lines on which the
	cubic form vanishes identically, that is, lines
	contained in the cubic surface.  The class of this
	locus is the top Chern class of this bundle.
   Example
	B = symmetricPower(3,Q)
	c = chern(rank B,B)
	integral c
   Text
	We can do the same thing for any n, (with d = 2n-3) as
	follows:
   Example
	f = n -> (
	     G := flagBundle({n-1,2});
	     integral chern symmetricPower_(2*n-3) last bundles G
	     )
	for n from 2 to 10 list time f n
   Text
	Note: in characteristic zero, using Bertini's theorem,
	the numbers computed can be proved to be equal
	to the actual numbers of distinct lines 
	for general hypersurfaces.  In $\PP^3$, every smooth cubic
	surface in characteristic zero has exactly 27 lines.
	In higher dimensions there may be smooth hypersurfaces for which
	the number of lines is different from the ``expected'' number
	that we have computed above.
	For example, the Fermat quintic threefold has an infinite number
	of lines on it.
  SeeAlso       
     "Conics on a quintic threefold"
--------
Node
  Key
     "Conics on a quintic threefold"       
  Headline
     an example
  Description
   Text
     The number of conics (rational curves of degree 2) on a general
     quintic hypersurface in $\PP^4$ was computed by S. Katz in 1985.  Here
     is how the computation can be made with {\em Schubert2}.

     Any conic in $\PP^4$ spans a unique plane, and the conics in a plane correspond to the points
     of $\PP^5$.  Hence the space of conics in
     $\PP^4$ is a certain $\PP^5$-bundle $X$ over the Grassmannian $G$ of planes in $\PP^4$.
   Example
     G = flagBundle{2,3}
   Text
     We extract the rank 2 tautological subbundle $S$ and the rank 3 tautological quotient bundle $Q$:
   Example
     (S,Q) = bundles G
   Text
     We form the bundle of quadratic forms on the variable planes:
   Example
     B = symmetricPower(2,Q)
   Text
     As a matter of convention, a {\em projective bundle} constructed by the function @ TO projectiveBundle' @
     in {\em Schubert2} parametrizes 
     rank 1 {\em quotients} of the sheaf provided.  The $\PP^5$-bundle of conics is given by sublinebundles of $B$, or
     equivalently, by rank 1 quotients of the dual, $B^*$, as in the following code:
   Example  
     X = projectiveBundle'(dual B, VariableNames => {,{z}})
   Text
     The equation of the general quintic is a section of the fifth symmetric
     power of the space of linear forms on $\PP^4$.  The induced equation on any given
     conic is an element in the corresponding closed fiber of a certain vector
     bundle $A$ of rank 11 on the parameter space $X$.  On any given plane $P$,
     and for any conic $C$ in $P$, we get the following exact sequence:
	   $$ 0 \to{} H^0(O_P(3)) \to{} H^0(O_P(5)) \to{} H^0(O_C(5)) \to{} 0$$
     As $C$ varies, these sequences glue to a short exact sequence of bundles on $X$:
	   $$ 0 \to{} Symm^3 Q \otimes O(-z) \to{} Symm^5 Q \to{} A \to{} 0$$
     We compute the class of $A$ in the Grothendieck group:
   Example
     A = symmetricPower_5 Q - symmetricPower_3 Q ** OO(-z)
   Text
     A given conic is contained in the quintic if and only if the equation of the
     quintic vanishes identically on the conic.  Hence the class of the locus of
     conics contained in the quintic is the top Chern class of $A$.  Hence
     the number of them is the integral of this Chern class:
   Example
     integral chern A
  SeeAlso
     "Lines on hypersurfaces"
-------
Node
  Key
    "A cubic fourfold containing a degree-5 del Pezzo surface"       
  Headline
    an example
  Description
    Text
      A smooth cubic fourfold {\tt Y} containing a degree-5 del Pezzo surface {\tt X} is known to be rational, see for example S. L. Tregub's "Three constructions of rationality of a four-dimensional cubic", 1984.  If {\tt H} is the hyperplane class on {\tt Y}, then {\tt 2H - X} is a linear series which gives a birational map from {\tt Y} to \mathbb{P}^4.  We will reproduce the numerical calculations which suggest (but do not prove) this fact.  We start by building part of the Chow ring of {\tt Y}:
    Example
      p = base(r,s)
      P5 = projectiveBundle(5,p)
      Y = sectionZeroLocus OO_P5(3) -- cubic fourfold
      x = chern(1,OO_Y(1)) -- hyperplane class
    Text
      We then build the Chow ring of the degree-5 del Pezzo:
    Example
      S = intersectionRing p -- important that we use the same base ring
      B1 = S[e_1..e_4,h, Join => false]
      I1 = (ideal vars B1)^3 -- relations imposed by dimension
      I2 = ideal flatten (for i from 0 to 4 list (for j from i+1 to 4 list (B1_i * B1_j))) -- relations imposed by non-intersection
      I3 = ideal for i from 1 to 4 list (e_i^2 + h^2) -- relations that make each exceptional divisor a (-1)-curve
      I = trim (I1 + I2 + I3)
      B = B1/I
      integral B := b -> coefficient((B_4)^2, b)
    Text
      We build the canonical class and tangent class of {\tt X}:
    Example
      K = -(3*h - e_1 - e_2 - e_3 - e_4)
      tX = 1 - K + 7*h^2
    Text
      The pullback map from {\tt Y} to {\tt X} takes the hyperplane class to the anticanonical class on {\tt X}.  Because a @ TO projectiveBundle @ has extra generators, we end up also having to say where powers of the hyperplane class map to:
    Example
      A = intersectionRing Y
      f = map(B, A, {K, -K, K^2, -K^3, K^4, -K^5})
    Text
      Now we build the @ TO inclusion @ of {\tt X} in {\tt Y}, which assembles the above information into a variety:
    Example
      i = inclusion(f,
         SubTangent => tX,
         SubDimension => 2,
         Base => p)
      X = source i
      Z = target i
    Text
      We blow up this inclusion so that we can work with the linear series {\tt 2H - X} as a divisor.
    Example
      (Ztilde, PN, PNmap, Zmap) = blowup(i)
    Text
      And now we calculate the Euler characteristic and degree of the line bundle {\tt 2H - E} on {\tt Ztilde}.
    Example
      AZtilde = intersectionRing Ztilde
      exc = chern(1,exceptionalDivisor Ztilde) -- exceptional class
      EBA = intersectionRing Z
      hyp = Zmap^* promote(x, EBA) -- hyperplane class of Ztilde
      L = OO_Ztilde(2*hyp - exc)
      chi L
      integral ((chern(1,L))^4)
    Text
      More generally, we can compute the Euler characteristic and degree of all line bundles of the form {\tt rH + sE} on {\tt Ztilde}:
    Example
      (r', s') = ((r_A, s_A) / (elt ->  promote(elt, EBA))) / Zmap^*
      L = OO_Ztilde(r' * hyp + s' * exc)
      chi L
      integral ((chern(1,L))^4)
--------
Node
  Key
    AbstractVariety
  Headline
    the class of all abstract varieties
  Description
   Text
     An abstract variety in {\em Schubert2} 
     is defined by its dimension and by a $\QQ$-algebra $A$, interpreted as the intersection ring.
     For example, the following code defines the abstract variety corresponding to $\PP^2$. 
   Example
     A=QQ[t]/ideal(t^3)
     X=abstractVariety(2,A)
   Text
     Once the variety $X$ is created, we can access its structure
     sheaf $O_X$ via the operator {\tt OO}, and view its Chern class:
   Example
     OO_X
     chern OO_X
   Text
     A variable of type @ TO AbstractVariety @ is implemented as @ ofClass MutableHashTable @, and can
     contain other information, such as the variety's tangent bundle, stored under the key @ TO TangentBundle @.
     Installation of a variety's tangent bundle enables the computation of its Todd class.
   Example
	X.TangentBundle  = abstractSheaf(X,Rank=>2, ChernClass=>(1+t)^3)
	todd X
   Text
     To enable the computation of such things as the Euler characteristic of a
     sheaf, we must also specify a method to take the @TO integral@ of an
     element of the intersection ring $A$; in the case where $A$ is Gorenstein,
     as is the case for the intersection ring modulo numerical equivalence of a
     complete nonsingular variety, the integral can often be implemented as the
     functional that takes the coefficient of the highest degree component with
     respect to a suitable basis of monomials.  The default integration method
     installed by such functions as @ TO base @ and @ TO abstractVariety @ for varieties
     of dimension greater than 0 returns
     a symbolic expression indicating the further integration that ought to be done.
     In this example, we choose to implement the integral by taking the coefficient of the
     monomial in our ring of top degree.
   Example
     integral A := f -> coefficient(t^2,f);
   Text
     Now we can compute the Euler characteristic of the line bundle whose first Chern class 
     is $2t$ (the algorithm uses the Todd class and the Riemann-Roch formula):
   Example
     chi OO_X(2*t)
   Text
      There are several other methods for constructing abstract varieties: the following functions
      construct basic useful varieties:
      @TO abstractProjectiveSpace@,
      @TO projectiveBundle@,
      @TO flagBundle@, and
      @TO base@.
  SeeAlso
    abstractVariety
    AbstractSheaf
    chern
    chi
    TangentBundle
    todd
--------
Node
  Key
    AbstractSheaf 
  Headline
    the class of all abstract sheaves
  Description
   Text
     A virtual {\em abstract sheaf} over @ofClass AbstractVariety@ $X$ is specified by its total Chern character.
-------
Node
  Key
   "The Horrocks-Mumford bundle"
  Headline
   an example
  Description
   Text
     The Horrocks-Mumford bundle on projective 4-space
     can be constructed with the following code.  We first produce a base point
     whose intersection ring contains a variable named {\tt n}, in 
     terms of which we can compute the Hilbert polynomial.
   Example
     pt = base(n)
   Text
     Then we create the projective space of dimension 4 over the base point.
   Example
    X = abstractProjectiveSpace'_4 pt
   Text
    Note that we use @TO abstractProjectiveSpace'@ to get Grothendieck-style notation.  This has the advantage that
    the first Chern class of the tautological line bundle is assigned to the variable {\tt h}:
   Example
     chern_1 OO_X(1)
   Text
     Now we create an abstract sheaf of rank 2 with $1 + 5 h + 10 h^2$ as its total Chern class:
   Example
     F = abstractSheaf(X, Rank => 2, ChernClass => 1 + 5*h + 10*h^2)
   Text
    Alternatively, we can use the representation of the Horrocks-Mumford bundle as the cohomology of the monad
    $$0 \rightarrow{} O_X(-1)^5 \rightarrow{} \Omega_X^2(2)^2 \rightarrow{} O_X^5 \rightarrow{} 0$$
    to produce a construction:
   Example
    F' = 2 * (exteriorPower_2 cotangentBundle X)(2) - 5 * OO_X(-1) - 5 * OO_X
    chern F'
    rank F'
   Text
    Here is the relationship between the two bundles:
   Example
    F === dual F'(-2)
   Text
     Now we compute the Hilbert polynomial of $F$.  This computation makes use of the Riemann-Roch Theorem.
   Example
    chi F(n*h)
--------
Node
  Key
    abstractProjectiveSpace'
    (abstractProjectiveSpace',ZZ,AbstractVariety)
    (abstractProjectiveSpace',ZZ)
    [abstractProjectiveSpace',VariableName]
  Headline
    make a projective space
  Usage
    abstractProjectiveSpace' n
    abstractProjectiveSpace'(n, S)
    abstractProjectiveSpace'_n S
  Inputs
    n : ZZ
    S : AbstractVariety
      if omitted, then @ TO point @ is used for it
    VariableName => Symbol
      the symbol to use for the variable representing the first Chern class of the tautological line bundle on the resulting projective space
  Outputs
    :
       the projective space of rank 1 quotient bundles of the trivial bundle of rank $n+1$ on
       the base variety {\tt S}.
  Description
   Example
     P = abstractProjectiveSpace' 3
     tangentBundle P
     chern tangentBundle P
     todd P
     chi OO_P(3)
   Text
     To compute the Hilbert polynomial of a sheaf on projective space, we work
     over a base variety of dimension zero whose intersection ring contains a
     {\em free} variable $n$, instead of working over @ TO point @:
   Example
     pt = base n
     Q = abstractProjectiveSpace'_4 pt
     chi OO_Q(n)
   Text
     The base variety may itself be a projective space:
   Example
     S = abstractProjectiveSpace'(4, VariableName => h)
     P = abstractProjectiveSpace'(3, S, VariableName => H)
     dim P
     todd P
  SeeAlso
     abstractProjectiveSpace
     projectiveBundle
     flagBundle
     tangentBundle
     todd
     chi
     base
     (symbol _,OO,AbstractVariety)
--------
Node
  Key
    abstractProjectiveSpace
    VariableName
    (abstractProjectiveSpace,ZZ,AbstractVariety)
    (abstractProjectiveSpace,ZZ)
    [abstractProjectiveSpace,VariableName]
  Headline
    make a projective space
  Usage
    abstractProjectiveSpace n
    abstractProjectiveSpace(n, S)
    abstractProjectiveSpace_n S
  Inputs
    n : ZZ
    S : AbstractVariety
      if omitted, then @ TO point @ is used for it
    VariableName => Symbol
      the symbol to use for the variable representing the first Chern class of the tautological line bundle on the resulting projective space
  Outputs
    :
       the projective space of rank 1 subbundles of the trivial bundle of rank $n+1$ on
       the base variety {\tt S}.
  Description
   Text
     Equivalent to flagBundle(\{1,n\},S,VariableNames=>\{h,\}).
   Example
     P = abstractProjectiveSpace 3
     tangentBundle P
     chern tangentBundle P
     todd P
     chi OO_P(3)
   Text
     The name is quite long.  Here is one way to make it shorter
   Example
     PP = abstractProjectiveSpace
     X = PP 4
   Text
     To compute the Hilbert polynomial of a sheaf on projective space, we work
     over a base variety of dimension zero whose intersection ring contains a
     {\em free} variable $n$, instead of working over @ TO point @:
   Example
     pt = base n
     Q = abstractProjectiveSpace_4 pt
     chi OO_Q(n)
   Text
     The base variety may itself be a projective space:
   Example
     S = abstractProjectiveSpace(4, VariableName => symbol h)
     P = abstractProjectiveSpace(3, S, VariableName => H)
     dim P
     todd P
  SeeAlso
     abstractProjectiveSpace'
     projectiveBundle
     flagBundle
     tangentBundle
     todd
     chi
     base
     (symbol _,OO,AbstractVariety)
--------
Node
  Key
    base
    Bundle
    (base,Sequence)
    (base,Thing)
  Headline
    make an abstract variety from nothing, equipped with some parameters and some bundles
  Usage
    X = base()
    X = base(p)
    X = base(d,p)
    X = base(d,p,...,Bundle=>(B,r,b),...)
  Inputs
    d:ZZ
      if omitted, the value $0$ is used
    p:Symbol
    B:Symbol
    r:ZZ
    b:
      either @ ofClass Symbol @, @ ofClass String @, @ ofClass List @ of @ TO2{Symbol,"symbols"} @, 
      @ TO2{IndexedVariable,"indexed variables"} @, or @ TO2{ChernClassVariable,"Chern class variables"} @ 
  Outputs
    :AbstractVariety
      a variety of dimension $d$ with an intersection ring containing specified variables
      and specified Chern classes of abstract sheaves
  Consequences
   Item
    Variables $p$, ..., of degree 0 are in the intersection ring of the variety.
    They are usable as (integer) variables (auxiliary parameters) in intersection ring computations.
   Item
    For each option {\tt Bundle=>(B,r,b)}, an abstract sheaf of rank {\tt r} is created and assigned to the variable {\tt B}.
    If {\tt b} is a symbol, then the Chern classes of $B$ are assigned to
    the indexed variables @ TT "b_1, ..., b_k" @, where $k = min(r,d)$.
    If {\tt b} is a string, {\tt "d"}, say, then the Chern classes of $B$ are assigned to 
    the variables whose names are {\tt d1}, {\tt d2}, {\tt d3}, ... .
    If {\tt b} is a list of length $k$, then the Chern classes are assigned to its elements.
   Item
    A default method for integration of elements of the intersection ring is installed, which returns a
    formal @ TO2{Expression,"expression"} @ representing the integral of the degree $d$ part of the element when $d$ is
    greater than zero, and simply returns the degree 0 part of the element when $d$ is zero.
  Description
   Text
    First we make a base variety and illustrate a computation with its two abstract sheaves:
   Example
     S = base(2,p,q, Bundle =>(A,1,a), Bundle => (B,2,"b"))
     intersectionRing S
     degrees oo
     chern (A*B)
     integral oo
   Text
    Then we make a projective space over it and use the auxiliary parameters {\tt p} and {\tt q} in a computation
    that checks the projection formula.
   Example
     X = abstractProjectiveSpace'(3,S,VariableName => H)
     intersectionRing X
     f = X.StructureMap
     x = chern f_* (f^* OO_S(p*a_1) * OO_X(q*H))
     y = chern f_* OO_X((f^*(p*a_1))+q*H)
     x == y
  SeeAlso
    abstractProjectiveSpace'
    base
    StructureMap
    chern
--------
Node
  Headline
    compute Chern classes of a sheaf
  Key
    chern
Node
  Key
    (chern,AbstractSheaf)
  Headline
    compute the total Chern class of a sheaf
  Usage
    chern A
  Inputs
    A:
  Outputs
    :
    	 the total Chern class of {\tt A}
  Description
    Example
      base(3, Bundle => (A,2,a), Bundle => (B,3,b))
      chern B
      chern(-A)
    Text
      The next example gives the total Chern class of a twist of
      a rank 2 vector bundle on the projective plane.
    Example
      pt = base(n,p,q)
      P2 = abstractProjectiveSpace'_2 pt
      E = abstractSheaf(P2, Rank=>2, ChernClass=>1+p*h+q*h^2)
      chern E(n*h)
  SeeAlso
     segre
Node
  Key
    (chern,ZZ,AbstractSheaf)
  Headline
     compute a Chern class of a sheaf
  Usage
    chern(n,A)
    chern_n A
  Inputs
    n:
    A:
  Outputs
    :
     the {\tt n}-th Chern class of {\tt A}
  Description
    Example
      base(3, Bundle => (A,2,a), Bundle => (B,3,b))
      chern_3 B
      chern(2,A**B)
  SeeAlso
     (segre,ZZ,AbstractSheaf)
--------
Node
  Key
    (chern,ZZ,ZZ,AbstractSheaf)
  Headline
    compute several Chern classes of an abstract sheaf 
  Usage
    chern(n,m,A)
  Inputs
    n :
    m :
    A :
  Outputs
    : List
    	 the list $ \{ c_n A .. c_m A \} $ of Chern classes of $A$
  Description
   Text
     In the following example, we consider two vector bundles
     {\tt A} and {\tt B} of ranks 2 and 3 on a variety of dimension 4.
   Example
     base(4,Bundle => (A,2,a), Bundle => (B,3,b))
     netList chern(2,4,B-A)
 SeeAlso
    base
    netList
--------
Node
  Key
      schubertCycle'
      (symbol _,FlagBundle,List)
      (symbol _,FlagBundle,Sequence)
      (schubertCycle',List,FlagBundle)
      (schubertCycle',Sequence,FlagBundle)
  Headline
      compute Schubert Cycles on a Grassmannian, Grothendieck-style
  Usage
     schubertCycle'(s,F)
     F_s
  Inputs
     F:FlagBundle
       associated to a vector bundle $A$, say, of rank $n$.
       This flag bundle should be a Grassmannian, parametrizing
       rank $q$ quotient bundles of $A$, say.
     s:
       @ ofClass{Sequence} @ $(s_0, ..., s_{q-1})$ of length $q$ of integers, with $0 \le{} s_0 < ... < s_{q-1} < n$,
       or
       @ ofClass{List} @  $\{s_0, ..., s_{q-1}\}$ of length $q$ of integers, with $n-q \ge{} s_0 \ge{} ... \ge{} s_{q-1} \ge{} 0$
  Outputs
     c:
       the appropriate {\em Schubert class} or {\em Schubert variety}, depending on the the type of {\tt s}.  See 
       page 271 of Fulton's book, {\em Intersection Theory} for the notation for @TO schubertCycle@, of which this
       is the dual.

       In the case where {\tt s} is a @ ofClass Sequence @, the value returned is the {\em homology} class of the {\em Schubert variety} in
       $ F $ consisting of those points corresponding to $(q-1)$-planes of $\PP(A)$ that meet $ W_j $ in dimension at least
       $ i $ with $0 \le{} i < q$, for each $ i $, where $ j = s_i $, and where $ W_j $ is the projective subspace of dimension
       $ j $ in a fixed (complete) flag $0 = W_0 \subset{} W_1 \subset{} ... \subset{} W_{n-1} = \PP(A) $.  Here $ \PP(A) $ denotes
       the modern Grothendieck-style projective bundle parametrizing rank 1 quotient bundles of $A$; for the older "Fulton-style" version,
       see @ TO schubertCycle @.        
       
       In the case where {\tt s} is a @ ofClass List @, the result is the corresponding {\em Schubert class} in
       cohomology.  In {\em Schubert2} homology and cohomology are identified with each other.
       Given a sequence $(s_0, ..., s_{q-1})$, one can convert it to a list yielding the same Schubert
       class by the formula $ \{..., n-q+i-s_i, ...\} $.
       
       This is related to @TO schubertCycle@ as follows: if $E'$ is the dual bundle of $A$ and $G' = G(q,A')$ is the dual
       Grassmannian, then {\tt schubertCycle'(s,G)} is carried to {\tt schubertCycle(s,G')} under the duality isomorphism.
  Description
   Example
     base(0, Bundle=>(A, n=8, a))
     F = flagBundle ({5,q=3},A)
     CH = intersectionRing F;
     F_(1,3,5)
     {n-q+0-1,n-q+1-3,n-q+2-5}
     F_oo
--------
Node
  Key
      schubertCycle
      (schubertCycle,List,FlagBundle)
      (schubertCycle,Sequence,FlagBundle)
  Headline
      compute Schubert Cycles on a Grassmannian, Fulton-style
  Usage
     schubertCycle(s,F)
  Inputs
     F:FlagBundle
       associated to a vector bundle $A$, say, of rank $n$.
       This flag bundle should be a Grassmannian, parametrizing
       rank $r$ sub-bundles of $A$, say.
     s:
       @ ofClass{Sequence} @ $(s_0, ..., s_{r-1})$ of length $r$ of integers, with $0 \le{} s_0 < ... < s_{r-1} < n$,
       or
       @ ofClass{List} @  $\{s_0, ..., s_{r-1}\}$ of length $r$ of integers, with $n-r \ge{} s_0 \ge{} ... \ge{} s_{r-1} \ge{} 0$
  Outputs
     c:
       the appropriate {\em Schubert class} or {\em Schubert variety}, depending on the the type of {\tt s}.  See 
       page 271 of Fulton's book, {\em Intersection Theory} for the notations.

       In the case where {\tt s} is a @ ofClass Sequence @, the value returned is the {\em homology} class of the {\em Schubert variety} in
       $ F $ consisting of those points corresponding to $(r-1)$-planes of $\PP(A)$ that meet $ W_j $ in dimension at least
       $ i $ with $0 \le{} i < r-1$, for each $ i $, where $ j = s_i $, and where $ W_j $ is the projective subspace of dimension
       $ j $ in a fixed (complete) flag $0 = W_0 \subset{} W_1 \subset{} ... \subset{} W_{n-1} = \PP(A) $.  Here $ \PP(A) $ denotes
       the "Fulton-style"  projective bundle parametrizing rank 1 sub-bundles of $A$; for opposite "Grothendieck-style" version, see
       @ TO schubertCycle' @.        
       
       In the case where {\tt s} is a @ ofClass List @, the result is the corresponding {\em Schubert class} in
       cohomology.  In {\em Schubert2} homology and cohomology are identified with each other.
       Given a sequence $(s_0, ..., s_{r-1})$, one can convert it to a list yielding the same Schubert
       class by the formula $ \{..., n-r+i-s_i, ...\} $.
  Description
   Example
     base(0, Bundle=>(A, n=8, a))
     F = flagBundle ({r=5,3},A)
     CH = intersectionRing F;
     schubertCycle((0,1,5,6,7),F)
     {n-r+0-0,n-r+1-1,n-r+2-5,n-r+3-6,n-r+4-7}
     schubertCycle(oo,F)
--------
Node
  Key
    (abstractSheaf,AbstractVariety,RingElement)
    (abstractSheaf,AbstractVariety,QQ)
    (abstractSheaf,AbstractVariety,ZZ)
    abstractSheaf
    ChernCharacter
    Rank
    ChernClass
    [abstractSheaf,Name]
    [abstractSheaf,Rank]
    [abstractSheaf,ChernClass]
    [abstractSheaf,ChernCharacter]
    (abstractSheaf,AbstractVariety)
  Headline
    make an abstract sheaf
  Usage
    abstractSheaf(X,cc,Rank=>n)
    abstractSheaf(X,Rank=>n)
    abstractSheaf(X,ChernClass=>c,Rank=>n)
    abstractSheaf(X,ChernCharacter=>cc)
  Inputs
    X:
    cc:
      an element of the intersection ring of {\tt X}
    Rank => RingElement
    ChernCharacter => RingElement
    ChernClass => RingElement
    Name => Thing
  Consequences
   Item
    The value of the {\tt Name} option should be a symbol or indexed variable to which the sheaf will be assigned.
    The function @ TO baseName @ will return it, so the sheaf can be used in situations where a symbol is expected.
    The sheaves created by @ TO base @ use this facility.
  Outputs
    :AbstractSheaf
      the abstract sheaf over {\tt X} with total Chern class {\tt c},
      rank {\tt n}, and total Chern character {\tt cc}.  When just the
      rank is specified, the Chern class is taken to be 1.  If the Rank and ChernCharacter options are both given,
      the value of the Rank option is ignored.  If
      the ChernClass and ChernCharacter options are both given, the values are used without checking for compatibility.
  Description
    Example
      X = abstractVariety(3, QQ[c])
      F = abstractSheaf(X, ChernCharacter => 3 + c)
      ch F
      chern F
--------
Node
  Key
    (abstractVariety,ZZ,Ring)
    abstractVariety
    [abstractVariety,ReturnType]
    ReturnType
    [abstractVariety,DefaultIntegral]
    DefaultIntegral
  Headline
    make an abstract variety
  Usage
    abstractVariety(d,A)
  Inputs
    d:
    A:
     of degree length 1
    ReturnType => Type
     a type of AbstractVariety
    DefaultIntegral => Boolean
  Outputs
   :
    of dimension {\tt d} whose intersection ring is {\tt A} and whose @ TO class @
    is the value of the ReturnType option
  Consequences
   Item
    The ring {\tt A} is altered so it knows what abstract variety it is associated with
    (see @ TO (variety,Ring) @),
    preventing it from being used in this way more than once.  The
    methods for converting its elements to strings and nets are replaced by methods that
    display monomials of higher degree further to the right, parenthesizing multiple
    terms of the same degree.
   Item
    Unless @ TO DefaultIntegral @ is set to false, a default method for integration of elements of the intersection ring is installed, which returns a
    formal @ TO2{Expression,"expression"} @ representing the integral of the degree $d$ part of the element when $d$ is
    greater than zero, and simply returns the degree 0 part of the element when $d$ is zero.
  Description
    Example
      X = abstractVariety(3, QQ[c,d,Degrees=>{1,2}])
      F = abstractSheaf(X, ChernCharacter => 3+c+d)
      chern F
      integral oo
--------
Node
  Key
    abstractVarietyMap
    (abstractVarietyMap,AbstractVariety,AbstractVariety,MethodFunction,MethodFunction)
    (abstractVarietyMap,AbstractVariety,AbstractVariety,RingMap,MethodFunction)
    [abstractVarietyMap, DefaultPullBack]
    DefaultPullBack
    [abstractVarietyMap, DefaultPushForward]
    DefaultPushForward
    [abstractVarietyMap, TangentBundle]
    [abstractVarietyMap, SectionClass]
  Headline
    make an abstract variety morphism
  Usage
    f = abstractVarietyMap(Y,X,fUpper,fLower)
  Inputs
    Y:AbstractVariety
      the target of $f$
    X:AbstractVariety
      the source of $f$
    fUpper:
      the method for pulling back from $Y$ to $X$; if given as a ring map, this should be a ring map from
      the intersection ring of $Y$ to that of $X$.  If given as a MethodFunction, there must be a method
      installed for pulling back elements of the intersection ring of $Y$ to that of $X$.  Optionally, a
      method can also be given for pulling back sheaves from $Y$ to $X$, in which case the
      {\tt DefaultPullBack => false} option must be set to override the default behavior.
    fLower:
      the method from pushing forward from $X$ to $Y$. Must have a method installed for pushing forward
      elements of the intersection ring of $X$ to that of $Y$.  Optionally, a method can also be given
      for pushing forward sheaves from $X$ to $Y$, in which case the {\tt DefaultPushForward => false}
      option must be set to override the default behavior.
    DefaultPullBack => Boolean
      if true, sheaf pullbacks are computed in the default manner from ring element pullbacks
    DefaultPushForward => Boolean
      if true, sheaf pushforwards are computed (if possible) in the default manner from ring element
      pushforwards
    TangentBundle => AbstractSheaf
      the relative tangent bundle of $f$.  If not supplied, is computed anyway if possible.
    SectionClass => RingElement
      the class of a section of $f$, an element of the intersection ring of $X$
  Outputs
   f:AbstractVarietyMap
     the resulting map from $X$ to $Y$
  Consequences
   Item
     If {\tt fUpper} is given as a ring map, then a new method is created for the pullback,
     encapsulating this ring map.  However, if {\tt fUpper} is given as a MethodFunction, then
     this MethodFunction is used directly, and may have new methods installed on it.
   Item
     If the {\tt DefaultPullBack => true} option is set (this is the default setting), then a
     method for pulling back sheaves is installed in the method fUpper.  The default behavior
     is to pull back both Chern classes and Chern characters via the supplied map.
   Item
     If the {\tt DefaultPullBack => true} option is set (this is the default setting),
     and if the calculation of sheaf pushforward is possible (i.e. either $X$ and $Y$ have supplied
     tangent bundles and $fUpper$ can pull back sheaves, or the {\tt TangentBundle} option is set),
     method for pulling back sheaves is installed in the method fUpper.  The default behavior is
     to compute the pushforward of sheaves via Grothendieck-Riemann-Roch.
  Description
    Text
      Typically it is inconvenient to have to use this function unless absolutely necessary.  Consider
      whether the morphism you need may be built via one of the built-in maps, for example via
      @ TO (map, FlagBundle, AbstractVariety, AbstractSheaf) @.
    Example
      X = point
      RX = intersectionRing X
      Y = abstractProjectiveSpace 3
      RY = intersectionRing Y
      fUpper = map(RX, RY, splice{4:0_RX})
      fLower = method()
      fLower RX := a -> promote(a,RY) * ctop last bundles Y;            
      incl = abstractVarietyMap(Y,X,fUpper,fLower)
      integral incl_* 1_RX
    Text
      This same example can be done much more easily via:
    Example
      X = point
      Y = abstractProjectiveSpace 3
      incl = map(Y,X,OO_X)
--------
Node
  Key
    AbstractVarietyMap
  Headline
    the class of all abstract variety maps
--------
Node
  Key
    adams
  Headline
    compute Adams operations
Node
  Key
    (adams,ZZ,AbstractSheaf)
  Usage
    adams(i,F)
    adams_i F
  Inputs
    i:
    F:
  Outputs
    :
     the effect of $i$-th Adams operation on $F$
  Description
    Example
      X = abstractVariety(3, QQ[c,d,e,Degrees=>{1,2,3}])
      F = abstractSheaf(X, ChernCharacter => 1 + c + d + e)
      adams_3 F
      ch oo
Node
  Key
    (adams,ZZ,RingElement)
  Usage
    adams(i,f)
    adams_i f
  Inputs
    i:
    f:
  Outputs
    :
     the Chern character of the effect of $i$-th Adams operation on the abstract sheaf whose Chern character is $f$
  Description
    Example
      X = abstractVariety(3, QQ[c,d,e,Degrees=>{1,2,3}])
      adams(3, 1 + c + d + e)
-------  
Node
  Key
    Base
  Headline
    get the base variety of a flag bundle
  Usage
    F.Base
  Inputs
    F:FlagBundle
  Outputs
    :AbstractVariety
      the base variety of $F$
  Description
    Example
      X = abstractProjectiveSpace 4
      X.Base
      Y = abstractProjectiveSpace_3 X
      Y.Base
--------
Node
  Key
    BundleRanks
  Headline
    get the ranks of the tautological sheaves on a flag bundle
  Usage
    F.BundleRanks
  Inputs
    F:FlagBundle
  Outputs
    :List
      the list of ranks of the tautological sheaves on $F$
  Description
    Example
      G = abstractProjectiveSpace 7
      G.BundleRanks
      X = flagBundle {1,2,3}
      X.BundleRanks
--------
Node
  Key
    Bundles
  Headline
    a symbol used internally as a key
  Usage
    F.Bundles
  Inputs
    F:FlagBundle
  Outputs
    :Sequence
      the list of the tautological sheaves on $F$
  Description
    Text
      The preferred way to get the list of tautological sheaves on $F$ is {\tt bundles F}.
    Example
      G = abstractProjectiveSpace 7
      G.Bundles
      rank \ oo
      X = flagBundle {1,2,3}
      X.Bundles
      rank \ oo
  SeeAlso
    bundles
--------
Node
  Key
    VarietyDimension
  Headline
    get the dimension of a variety from its intersection ring
  Usage
    A.VarietyDimension
  Inputs
    A:Ring
      the intersection ring of an abstract variety
  Description
    Example
      X = abstractProjectiveSpace 3
      A =intersectionRing X
      A.VarietyDimension
--------
Node
  Key
    TautologicalLineBundle
  Headline
    a symbol used internally as a key
  Description
    Text
      If $X$ is an abstract variety and its tautological line bundle has already been built,
      it is stored in {\tt X.TautologicalLineBundle}.  However, because building this bundle
      can be computationally intensive, it is often not built unless it is requested by
      @TO tautologicalLineBundle@.  The programmer is warned not to make direct reference to this
      key for this reason.
    Example
      X = abstractProjectiveSpace 3
      try X.TautologicalLineBundle else print "bundle not found"
      L = tautologicalLineBundle X
      L === X.TautologicalLineBundle
      rank L
      chern L
    Text
      Here are the preferred ways to get the tautological line bundle:
    Example
      OO_X(1)
      L === oo
      tautologicalLineBundle X
      L === oo
  SeeAlso
    tautologicalLineBundle
--------
Node
  Key
    ch
  Headline
    Chern character of an abstract sheaf
Node
  Key
    (ch,AbstractSheaf)
  Headline
    total Chern character of an abstract sheaf
  Usage
   ch F
  Inputs
   F:
  Outputs
   :
    the total Chern character of $F$
  Description
    Example
     X = abstractProjectiveSpace' 3
     L = OO_X(1)
     chern L
     ch L
Node
  Key
    (ch,ZZ,AbstractSheaf)
  Headline
    the i-th Chern character of an abstract sheaf
  Usage
   ch F
  Inputs
   i:
   F:
  Outputs
   :
    the $i$-th Chern character of $F$
  Description
    Example
     X = abstractProjectiveSpace' 3
     L = OO_X(1)
     chern L
     ch_1 L, ch_2 L, ch_3 L
--------
Node
  Key
    ChernClassVariable
  Headline
    the class of all Chern class variables
  Description
   Text
    This class supports the creation of variables, analogous to @ TO2{IndexedVariable,"indexed variables"} @,
    that can be used as generators of intersection rings
    of abstract varieties that are displayed in the form {\tt c_iE}, where {\tt i} is a natural number
    and {\tt E} is the name of an abstract sheaf.
   Example
    F = flagBundle({3,2},VariableNames => {{chern_1 S .. chern_3 S},{chern_1 Q,chern_2 Q}})
    A = intersectionRing F
    (S,Q) = bundles F
    chern Q
    chern S
  SeeAlso
   ChernClassVariableTable
--------
Node
 Key
  (symbol ..,ChernClassVariable,ChernClassVariable)
 Headline
  generate a sequence of Chern class variables
 Usage
  v .. w
 Inputs
  v:
   equal to @ TT "chern_i E" @, say
  w:
   equal to @ TT "chern_j E" @, say
 Outputs
  :Sequence
   the sequence whose elements are the Chern class variables @ TT "chern_k E" @ as k ranges from i to j
 Description
  Example
   chern_1 E .. chern_10 E
--------
Node
  Key
    (chi,AbstractSheaf)
  Headline
    compute the Euler characteristic of an abstract sheaf
  Usage
   chi F
  Inputs
   F:
  Outputs
   :
    the Euler characteristic of $F$
  Description
   Example
    X = abstractProjectiveSpace' 2
    chi OO_X(-1), chi OO_X(0), chi OO_X(1), chi OO_X(2), chi OO_X(3)
    Y = abstractProjectiveSpace'(2, base n)
    chi OO_Y(n)
   Text
    The algorithm uses the Hirzebruch-Riemann-Roch theorem.
--------
Node
  Key
    (ctop,AbstractSheaf)
    ctop
  Headline
   the top Chern class of an abstract sheaf
  Usage
   ctop F
  Inputs
   F:
  Outputs
   :
    the top Chern class of $F$
  Description
   Text
    Here we compute the top Chern class of a vector bundle of rank 6 on the way toward getting
    the number of lines on quintic threefold.
   Example
    G = flagBundle{3,2}
    B = symmetricPower_5 last bundles G
    ctop B
    degree oo
    integral ooo
--------
Node
  Key
    FlagBundle
  Headline
    the class of all flag bundles
--------
Node
  Key
   Isotropic
  Headline
   make an isotropic flag bundle
  Description
   Text
    {\tt Isotropic} is the name for an optional argument that directs the construction
    of an isotropic flag bundle.
--------
Node
  Key
    flagBundle
    VariableNames
    (flagBundle,List)
    (flagBundle,List,AbstractSheaf)
    (flagBundle,List,AbstractVariety)
    (flagBundle,List,ZZ)
    [flagBundle,QuotientBundles]
    [flagBundle,VariableNames]
    [flagBundle,Isotropic]
  Headline
   make a flag bundle
  Usage
   flagBundle(r,E)
  SeeAlso
   abstractProjectiveSpace
   projectiveBundle
  Inputs
   r:List
    a list $\{r_0, ..., r_{n-1}\}$ of non-negative integers
   E:AbstractSheaf
    whose flag bundle is to be formed.  
    If $E$ is, instead, @ ofClass AbstractVariety @ then a trivial sheaf on it is used.  
    If $E$ is, instead, @ ofClass ZZ @ then a trivial sheaf on @ TO point @ of rank $E$ is used.  
    If $E$ is omitted, then a trivial sheaf on @ TO point @ is used; its rank is the sum of the entries of $r$.
   VariableNames => Thing
    used for specifying how the generators of the intersection ring of the resulting flag bundle are to be named.
    If no value is provided, then @ TT "H_(i,j)" @ = $H_{i,j}$ denotes the j-th Chern class of i-th tautological sheaf.
    If its value is a symbol, then that symbol is used instead of {\tt H}.
    If its value is a list, then for each i, its i-th entry specifies how the Chern classes of the i-th tautological sheaf are to be named.
    If the i-th entry is omitted (i.e., is @ TO null @), then the default names are used.
    If the i-th entry is a symbol, say {\tt c}, then the names are @ TT "c_1 .. c_(r_i)" @.
    If the i-th entry is a string, say {\tt c}, then the names are {\tt c1}, {\tt c2}, {\tt c3}, ... .
    If the i-th entry is a list, its entries are used as the names.
   QuotientBundles => Boolean
    whether the additional tautological subquotient bundle, needed to make up the discrepancy between the
    sum of the rank specified by $r$ and the rank of $E$, should be at the bottom of the filtration.
   Isotropic => Boolean
    whether to produce the isotropic flag bundle; in that case, the vector bundle should have even rank.
  Outputs
   F:FlagBundle
    the flag bundle over the variety of $E$ parametrizing filtrations $0 = E_0 \subseteq{} E_1 \subseteq{}
    ... \subseteq{} E_n = E$ of $E$ whose successive subquotients are vector bundles $E_{i+1}/E_i$
    of rank $r_i$
  Consequences
   Item
    The list {\tt r} of ranks can be obtained later with {\tt F.BundleRanks}, see @ TO BundleRanks @.
    Its sum can be obtained with {\tt F.Rank}.
   Item
    The list of consecutive quotients in the tautological filtration can be obtained later with {\tt bundles F}, see @ TO bundles @.
    See also @ TO SubBundles @ and @ TO QuotientBundles @.
   Item
    The (relative) tautological line bundle can be obtained later with {\tt OO_F(1)}.
   Item
    The structure map from {\tt F} to the variety of {\tt E} can be obtained later with {\tt F.StructureMap}, see @ TO StructureMap @.
    Abstract sheaves and cycle classes can be pulled back and pushed forward, see @ TO (symbol ^*, AbstractVarietyMap) @
    and  @ TO (symbol _*, AbstractVarietyMap) @.  Integration will work if it works on the variety of {\tt E}, see @ TO integral @.
    The (relative) tangent bundle can be obtained from it, see @ TO (tangentBundle,AbstractVarietyMap) @.
  Description
    Example
      base(3,Bundle => (E,4,c))
      F = flagBundle({2,2},E)
      bundles F
      rank \ oo
      chern \ ooo
      product toList oo
      intersectionRing flagBundle({2,2},E,VariableNames=>{{a,b},t})
--------
Node
  Key
    projectiveBundle'
    (projectiveBundle',AbstractSheaf)
    (projectiveBundle',ZZ)
    (projectiveBundle',ZZ,AbstractVariety)
    [projectiveBundle',VariableNames]
  Headline
   make a projective bundle from an abstract sheaf
  SeeAlso
   projectiveBundle
   abstractProjectiveSpace
   flagBundle
  Usage
    projectiveBundle' F
    projectiveBundle'(n,X)
    projectiveBundle' n
  Inputs
    F : AbstractSheaf
    n : ZZ
    X : AbstractVariety
    VariableNames => Thing
      see @ TO [flagBundle,VariableNames] @
  Outputs
    :
       the projective bundle of rank 1 quotient bundles of the abstract sheaf $F$.
       If $F$ is omitted, then a projective space of dimension $n$ over $X$ is produced.
       If $X$ is omitted, then @ TO point @ is used as the base.
  Description
   Example
    X = projectiveBundle' 4
    F = OO_X(2) ++ OO_X(3) ++ OO_X(4)
    Y = projectiveBundle' F
    dim Y
    integral (chern_1 OO_Y(1))^(dim Y)
    bundles X/rank
  Caveat
   Perhaps this should be merged with @ TO abstractProjectiveSpace @.  (The optional arguments are slightly different.)
--------
Node
  Key
    projectiveBundle
    (projectiveBundle,AbstractSheaf)
    (projectiveBundle,ZZ)
    (projectiveBundle,ZZ,AbstractVariety)
    [projectiveBundle,VariableNames]
  Headline
   make a projective bundle from an abstract sheaf
  SeeAlso
   projectiveBundle'
   abstractProjectiveSpace
   flagBundle
  Usage
    projectiveBundle F
    projectiveBundle(n,X)
    projectiveBundle n
  Inputs
    F : AbstractSheaf
    n : ZZ
    X : AbstractVariety
    VariableNames => Thing
      see @ TO [flagBundle,VariableNames] @
  Outputs
    :
       the projective bundle of rank 1 sub-bundles of the abstract sheaf $F$.
       If $F$ is omitted, then a projective space of dimension $n$ over $X$ is produced.
       If $X$ is omitted, then @ TO point @ is used as the base.
  Description
   Example
    X = projectiveBundle 4
    F = OO_X(2) ++ OO_X(3) ++ OO_X(4)
    Y = projectiveBundle F
    dim Y
    integral (chern_1 OO_Y(1))^(dim Y)
    bundles X/rank
  Caveat
   Perhaps this should be merged with @ TO abstractProjectiveSpace @.  (The optional arguments are slightly different.)
--------
Node
  Key
   blowup
   (blowup, AbstractVarietyMap)
  Headline
   blow up an abstract variety along a subvariety
  Usage
   (Ytilde, PN, PNmap, Ymap) = blowup(i)
  SeeAlso
   exceptionalDivisor
  Inputs
   i:AbstractVarietyMap
    the inclusion map of the subvariety $X$ to be blown up into the ambient variety $Y$
  Outputs
   Ytilde:AbstractVariety
    the blowup of $Y$ along $X$
   PN:FlagBundle
    the exceptional divisor of the blowup, the projectivization of the normal bundle of $i$
   PNmap:AbstractVarietyMap
    the inclusion of $PN$ into the blowup
   Ymap:AbstractVarietyMap
    the map from the blowup down to $Y$
  Consequences
   Item
    The exceptional divisor as a sheaf on the blowup can be obtained via {\tt exceptionalDivisor Ytilde}, see @ TO exceptionalDivisor @.
  Description
    Text
      Blowing up a point in $\mathbb{P}^2$:
    Example
      X = abstractProjectiveSpace 0
      Y = abstractProjectiveSpace 2
      i = map(Y,X,OO_X)
      (Ytilde, PN, PNmap, Ymap) = blowup(i)
      Ediv = chern(1, exceptionalDivisor Ytilde) -- the class of the exceptional divisor
      integral (Ediv^2)
    Text
      As a more interesting example, we can derive the classical formula for the degree of the zero-dimensional locus of intersection of three
      surfaces in $\mathbb{P}^3$ containing a twisted cubic:
    Example
      B = base(r,s,t)
      X = abstractProjectiveSpace(1, B)
      Y = abstractProjectiveSpace(3, B)
      i = map(Y,X,OO_X(3)) --includes P^1 into P^3 as the twisted cubic
      (Ytilde, PN, PNmap, Ymap) = blowup(i)
      Ediv = chern(1, exceptionalDivisor Ytilde)
      hyperplane = chern(1,OO_Y(1))
      (rsurf, ssurf, tsurf) = (x -> hyperplane * x) \ (r,s,t) --classes of surfaces of degrees r,s,t
      (ptr, pts, ptt) = (x -> (Ymap^* x) - Ediv) \ oo --proper transforms of each surface
      integral(ptr * pts * ptt)
    Text
      The file {\tt Schubert2/blowup-test.m2} has several more examples.
--------
Node
  Key
    exceptionalDivisor
    (exceptionalDivisor,AbstractVariety)
  Headline
    get the exceptional divisor of a blowup
  Usage
    exceptionalDivisor X
  Inputs
    X:AbstractVariety
     a blowup, built using @ TO blowup @
  Outputs
    :AbstractSheaf
      the exceptional divisor on X, expressed as a line bundle
  SeeAlso
    blowup
  Description
    Example
      T = abstractProjectiveSpace 2
      S = abstractProjectiveSpace 0
      i = map(T,S,OO_S) -- inclusion of a point in P^2
      X = first blowup(i)
      exceptionalDivisor X
      chern oo
--------
Node
  Key
    extensionAlgebra
    (extensionAlgebra,RingMap,RingElement)
    [extensionAlgebra,Codimension]
    [extensionAlgebra,CoefficientRing]
  Headline
    extend a graded algebra by a graded module
  Usage
    extensionAlgebra(f,c)
    extensionAlgebra(f,c,Codimension => r)
    extensionAlgebra(f,c,CoefficientRing => S)
    extensionAlgebra(f,c,Codimension => r, CoefficientRing => S)        
  Inputs
    f:
      from {\tt A} to {\tt B}
    c:
      a homogeneous element of {\tt B}
    Codimension => ZZ
      the desired degree of the inclusion map from {\tt B} to {\tt E}; only required if {\tt c = 0}, and otherwise must match degree of {\tt c}
    CoefficientRing => Ring
      a coefficient ring of A, which will be used as the coefficient ring of the output
  Consequences
   Item
     An algebra {\tt E} is created and promotion methods from {\tt A} and {\tt B} to {\tt E} and vice-versa are installed.  The natural pullback map from {\tt E} to {\tt B} given by sending {\tt a+b} to {\tt f(a) + cb} is stored in {\tt E.PullBack}, see @ TO PullBack @.
  Outputs
    :Ring
      the algebra obtained by making the direct sum of {\tt A} and {\tt B[-r]} into an algebra with the multiplication rule {\tt (a+b)(a'+b') = aa' + ab' + a'b + cbb'}.
  Description
    Example
      A = QQ[x]
      B = QQ[y]
      c = 2_B
      f = map(B,A,gens B)
      extensionAlgebra(f,c)
      oo.PullBack
--------
Node
  Key
    inclusion
    (inclusion,RingMap)
    [inclusion,Codimension]
    [inclusion,NormalClass]
    NormalClass
    [inclusion,SubDimension]
    SubDimension
    [inclusion,SuperDimension]
    SuperDimension
    [inclusion,SubTangent]                
    SubTangent
    [inclusion,SuperTangent]    
    SuperTangent
    [inclusion,Base]
  Headline
    build the freest possible inclusion map
  Usage
    inclusion(f, NormalClass => c, Codimension => r, SuperTangent => tY, SuperDimension => dY, Base => S)
    inclusion(f, SubTangent => tX, SubDimension => dX, SuperTangent => tY, SuperDimension => dY)    
    inclusion(f)
  Inputs
    f:
      from {\tt A} to {\tt B}; we think of {\tt A} as part of the Chow ring of {\tt Y} and {\tt B} as part of the Chow ring of {\tt X}
    Codimension => ZZ
      the codimension of {\tt X} in {\tt Y}
    SubDimension => ZZ
      the dimension of {\tt X}
    SuperDimension => ZZ
      the dimension of {\tt Y}
    SubTangent => RingElement
      element of {\tt B}, the Chern class of the tangent bundle of {\tt X}
    SuperTangent => RingElement
      element of {\tt A}, the Chern class of the tangent bundle of {\tt Y}
    NormalClass => RingElement
      element of {\tt B}, the Chern class of the normal bundle of {\tt X} in {\tt Y}
    Base => Thing
      a Ring or AbstractVariety, the base ring/variety to work over
  Consequences
   Item
     An @ TO extensionAlgebra @ of {\tt A} by {\tt B} is built, with normal class equal to the degree-{\tt r} part of {\tt c}.  This algebra is made into the ring of a variety {\tt Z}, see @ TO abstractVariety @.  A tangent class is installed on {\tt Z}, see @ TO tangentBundle @.  If {\tt A} and {\tt B} each have an @ TO integral @ defined, then an integral is defined on the coordinate ring of {\tt Z}, and a @ TO StructureMap @ is installed on {\tt Z}.
   Item
     A variety {\tt X} for {\tt B} is built if {\tt B} is not already the ring of a variety, see @ TO abstractVariety @.  A tangent class is installed on {\tt X} if it does not already have one, see @ TO tangentBundle @.  A @ TO StructureMap @ is installed if {\tt B} already has an @ TO integral @ defined.
   Item
     If {\tt Base} option is used, the supplied ring is made into the ring of a variety, if it is not already one.
  Outputs
    :AbstractVarietyMap
      the natural map from {\tt X} to {\tt Z}.
  Description
    Text
      Given the pullback map {\tt f} from {\tt A} to {\tt B}, builds the freest possible extension {\tt E} of {\tt A} by {\tt B} (see @ TO extensionAlgebra @), and then adds appropriate metadata to make the maps from {\tt E} to {\tt B} and vice-versa into an @ TO AbstractVarietyMap @.  Enough information must be given to compute the dimensions of {\tt X} and {\tt Y}, either by using the SubDimension, SuperDimension, and Codimension optons, or by having varieties already attached to {\tt A} and/or {\tt B}.  Likewise, enough information must be given to compute the tangent classes of {\tt X} and {\tt Y}.
    Text
      This construction is useful for computations where the pullback map is known but the pushforward is either not known or cannot be defined.
    Example
      -- Building the inclusion of the Veronese in P5, the hard way
      p = point
      S = intersectionRing p
      Y = projectiveBundle(5,p)
      A = intersectionRing Y      
      B = S[h, Join => false]/h^3 -- A^*(P2), but using 2 times a line as the generating class:
      integral B := (b) -> (4 * coefficient((B_0)^2, b))
      c = 1 + (9/2)*h + (15/2)*h^2 -- normal class
      f = map(B,A,{-h, h, h^2, h^3, h^4, h^5})
      i = inclusion(f,
          NormalClass => c,
          Codimension => 3,
	  Base => p) -- Base not necessary, will be correctly computed
      Z = target i
      X = source i
      Xstruct = X / point
      rank Xstruct_* tangentBundle X
      integral chern tangentBundle Z
--------
Node
  Key
   integral
  Headline
   compute an integral (push forward a cycle to the base)
  Usage
   integral f
  Inputs
   f:RingElement
    a cycle class in the intersection ring of an abstract variety
  Outputs
   :RingElement
    the integral of $f$.
  Description
   Text
    In this example we compute the number of lines meeting four lines in space.
   Example
    G = flagBundle {2,2}
    dim G
    A = intersectionRing G
    f = (chern_1 OO_G(1))^4
    integral f
   Text
    Normally this integral will be an element of the intersection ring of
    @ TO point @, and thus will essentially be a rational number, but it could be in any base variety,
    and would be obtained by pushing forward along the structure maps of flag bundles until
    the (ultimate) base variety is reached.
   Example
    pt = base n
    F = flagBundle_{2,2} pt
    integral (chern_1 OO_F(n))^4
--------
Node
  Key
   IntersectionRing
  Headline
   a key used for storing the intersection ring in an abstract variety
  Description
   Text
    This symbol is used as a key internally.
  SeeAlso
   intersectionRing
--------
Node
  Key
    (intersectionRing,AbstractVariety)
    intersectionRing
  Headline
   get the intersection ring of an abstract variety
  Usage
   intersectionRing X
  Inputs
   X:
  Outputs
   :
    the intersection ring of $X$
  Description
   Example
    intersectionRing abstractProjectiveSpace' 3
   Text
    The variables may not have been assigned their values in the intersection ring yet:
   Example
    H_(1,1)
   Text
    The function @ TO use @ will arrange for the assignment of values:
   Example
    use ooo
    H_(1,1), H_(1,2), H_(1,3)
  SeeAlso
   abstractProjectiveSpace'
   IntersectionRing
--------
Node
  Key
   PullBack
  Headline
   a symbol used internally as a key for storing the pull back map in an abstract variety map
  SeeAlso
   (symbol ^*,AbstractVarietyMap)
   extensionAlgebra
--------
Node
  Key
    (schur,List,AbstractSheaf)
    schur
  Headline
   apply a Schur functor to an abstract sheaf
  Usage
   schur(p,E)
   schur_p E
  Inputs
   p:
    a partition, specified by a descending list of non-negative integers
   E:
  Outputs
   :
    the result of applying the Schur functor corresponding to the partition $p$ to $E$
  Description
   Example
    base(3, Bundle => (E,3,c))
    chern E
    chern schur_{1,1,1} E
    chern schur_{2,1} E
    chern schur_{3} E
  SeeAlso
   (exteriorPower,ZZ,AbstractSheaf)
   (symmetricPower,RingElement,AbstractSheaf)
--------
Node
 Key
  (exteriorPower,AbstractSheaf)
 Usage
  exteriorPower F
 Headline
  alternating sum of exterior powers
 Inputs
  F:
   which should be {\em effective}
 Outputs
  :
 Description
  Example
   base(7, Bundle => (E,4,c))
   E
   exteriorPower E
   chern oo
--------
Node
  Key
   (exteriorPower,ZZ,AbstractSheaf)
  Usage
   exteriorPower(n,F)
   exteriorPower_n F
  Inputs
   n:
   F:
  Outputs
   :
    the $n$-th exterior power of $F$
  Description
   Example
    tangentBundle abstractProjectiveSpace 4
    exteriorPower_4 oo
    chern oo
  SeeAlso
   (symmetricPower,RingElement,AbstractSheaf)
   (schur,List,AbstractSheaf)
   (determinant,AbstractSheaf)
--------
Node
  Key
   (symmetricPower,RingElement,AbstractSheaf)
  Headline
   symmetric power of an abstract sheaf
  Usage
   symmetricPower(n,F)
   symmetricPower_n F
  Inputs
   n:
   F:
  Outputs
   :
    the $n$-th symmetric power of $F$
  Description
   Text
    In the first example, we let $n$ be a natural number.
   Example
    tangentBundle abstractProjectiveSpace 4
    symmetricPower_4 oo
    chern oo
   Text
    In the next example, we let $n$ be a free parameter in the ``intersection ring'' of the base variety.
   Example
    pt = base n
    X = abstractProjectiveSpace'_2 pt
    tangentBundle X
    F = symmetricPower_n oo
    chern F
    ch F
    chi F
  SeeAlso
   (exteriorPower,ZZ,AbstractSheaf)
   (schur,List,AbstractSheaf)
--------
Node
  Key
   SectionClass
  Headline
   a symbol used internally as a key
  SeeAlso
   sectionClass
--------
Node
  Key
   (sectionClass,AbstractVarietyMap)
   sectionClass
  Headline
   get the class of the image of a section of a map of abstract varieties
  Usage
   sectionClass f
  Inputs
   f:
  Outputs
   :
    the class of a section of $f$.  (This means, more or less, any cycle class of relative 
    dimension zero whose integral is 1.)
  Description
   Example
    X = abstractProjectiveSpace 4
    f = X.StructureMap
    sectionClass f
    integral oo
    G = flagBundle {2,2}
    G.StructureMap
    sectionClass oo
    integral oo
--------
Node
  Headline
    compute Segre classes of a sheaf
  Key
    segre
Node
  Key
    (segre,AbstractSheaf)
  Headline
    compute the total Segre class of a sheaf
  Usage
    segre A
  Inputs
    A:
  Outputs
    :
     the total Segre class of {\tt A}, defined as the reciprocal of the chern class of the {\em dual} of {\tt A}.  (In
     a future version, the dual may be omitted, and an alternative function {\tt segre'} may be introduced.)
  Description
    Example
      base(4, Bundle => (B,3,b))
      chern B
      segre B
  SeeAlso
     chern
Node
  Key
    (segre,ZZ,AbstractSheaf)
  Headline
     compute a Segre class of a sheaf
  Usage
    segre(n,A)
    segre_n A
  Inputs
    n:
    A:
  Outputs
    :
     the {\tt n}-th Segre class of {\tt A}
  Description
    Example
      base(3, Bundle => (A,2,a), Bundle => (B,3,b))
      segre_3 B
      segre(2,A**B)
  SeeAlso
     (chern,ZZ,AbstractSheaf)
--------
Node
  Key
   StructureMap
  Headline
   get the structure map of an abstract variety
  Usage
   F.StructureMap
  Inputs
   F:AbstractVariety
  Outputs
   :AbstractVarietyMap
    the structure map of $F$, if it has one.
  SeeAlso
   (symbol /,AbstractVariety,AbstractVariety)
  Description
   Example
    F = abstractProjectiveSpace 2
    f = F.StructureMap
    source f
    f_* OO_F
    f_* OO_F(1)
    target f
    f^* OO_point
--------
Node
  Key
   TangentBundle
  Headline
   a symbol used internally as a key
  SeeAlso
   tangentBundle
--------
Node
  Key
    tangentBundle
    (tangentBundle,AbstractVariety)
    (tangentBundle,AbstractVarietyMap)
  Headline
   get the tangent bundle
  SeeAlso
   cotangentBundle
  Usage
   tangentBundle X
   tangentBundle f
  Inputs
   X:AbstractVariety
   f:AbstractVarietyMap
  Outputs
   :AbstractSheaf
    the (absolute) tangent bundle of $X$ or the (relative) tangent bundle of $f$
  Description
   Example
    X = abstractProjectiveSpace' 2
    Y = abstractProjectiveSpace'_2 X
    tangentBundle Y
    chern oo
    tangentBundle Y.StructureMap
    chern oo
-------
Node
  Key
    cotangentBundle
    (cotangentBundle,AbstractVariety)
    (cotangentBundle,AbstractVarietyMap)
  Headline
   get the cotangent bundle
  SeeAlso
   tangentBundle
  Usage
   cotangentBundle X
   cotangentBundle f
  Inputs
   X:AbstractVariety
   f:AbstractVarietyMap
  Outputs
   :AbstractSheaf
    the (absolute) cotangent bundle of $X$ or the (relative) cotangent bundle of $f$
  Description
   Example
    X = abstractProjectiveSpace' 2
    Y = abstractProjectiveSpace'_2 X
    cotangentBundle Y
    chern oo
    cotangentBundle Y.StructureMap
    chern oo
--------
Node
  Key
    todd
    (todd,AbstractSheaf)
    (todd,AbstractVariety)
    (todd,AbstractVarietyMap)
  Headline
   compute the Todd class of an abstract sheaf, variety, map
  Usage
   todd F
   todd X
   todd f
  Inputs
   F:AbstractSheaf
   X:AbstractVariety
   f:AbstractVarietyMap
  Outputs
   :RingElement
    the Todd class of $F$, of the (absolute) tangent bundle of $X$,
    of the (relative) tangent bundle of $f$
  Description
   Example
    todd abstractProjectiveSpace 6
--------
Node
  Key
   SubBundles
  Headline
   get the subbundles of the tautological filtration on a flag bundle
  Usage
   F.SubBundles
  Inputs
   F:FlagBundle
  Outputs
   :
    @ ofClass List @ whose elements are the subbundles $0 = E_0 \subseteq{} E_1 \subseteq{} ... \subseteq{} E_n = f^* B$
    in the tautological filtration of $f^* B$, where $F$ is a flag bundle parametrizing filtrations of an abstract sheaf
    $B$ on a variety $X$, and $f : F \rightarrow X$ is the structure map of $F$    
  Description
   Example
    F = flagBundle {1,1,2}
    F.SubBundles
    rank \ oo
    netList \\ chern \ ooo
--------
Node
  Key
   QuotientBundles
  Headline
   get the quotient bundles of the tautological filtration on a flag bundle
  Usage
   F.QuotientBundles
  Inputs
   F:FlagBundle
  Outputs
   :
    @ ofClass List @ $\{E_n/E_n, E_n/E_{n-1}, ..., E_n/E_1, E_n/E_0\}$
    whose elements are the quotient bundles 
    in the tautological filtration $0 = E_0 \subseteq{} E_1 \subseteq{} ... \subseteq{} E_n = f^* B$,
    where $F$ is a flag bundle parametrizing filtrations of an abstract sheaf
    $B$ on a variety $X$, and $f : F \rightarrow X$ is the structure map of $F$    
  Description
   Example
    F = flagBundle {1,1,2}
    F.QuotientBundles
    rank \ oo
    netList \\ chern \ ooo
--------
Node
  Key
   point
  Headline
   the default base variety of dimension 0
  Usage
   point
  Outputs
   :AbstractVariety
    the default base variety of dimension 0, with intersection ring isomorphic to @ TO QQ @
  Description
   Example
    point
    dim point
    intersectionRing point
    X = abstractProjectiveSpace 4
    f = X.StructureMap
    target f
--------
Node
  Key
   (symbol *,AbstractSheaf,AbstractSheaf)
   (symbol **,AbstractSheaf,AbstractSheaf)
   (symbol *,QQ,AbstractSheaf)
   (symbol *,RingElement,AbstractSheaf)
   (symbol *,AbstractSheaf,QQ)
   (symbol *,AbstractSheaf,RingElement)
   (symbol *,AbstractSheaf,ZZ)
   (symbol *,ZZ,AbstractSheaf)
  Headline
   (tensor) product of two abstract sheaves
  Usage
   F*G
   F**G
  Inputs
   F:
   G:
  Outputs
   :
    the (tensor) product of $F$ and $G$
  Description
   Text
    If {\tt F} or {\tt G} is a ring element of degree 0, it represents a trivial bundle of that rank.
   Example
    X = abstractProjectiveSpace 1
    OO_X(1) * OO_X(2)
    chi oo
   Text
    The sheaves can be on different varieties if one of the varieties is over the other.
--------
Node
  Key
    (symbol +,AbstractSheaf,AbstractSheaf)
    (symbol ++,AbstractSheaf,AbstractSheaf)
    (symbol +,AbstractSheaf,ZZ)
    (symbol +,ZZ,AbstractSheaf)
    (symbol ++,AbstractSheaf,ZZ)
    (symbol ++,ZZ,AbstractSheaf)
    (symbol ++,AbstractSheaf,QQ)
    (symbol ++,AbstractSheaf,RingElement)
    (symbol ++,QQ,AbstractSheaf)
    (symbol ++,RingElement,AbstractSheaf)
    (symbol +,AbstractSheaf,QQ)
    (symbol +,AbstractSheaf,RingElement)
    (symbol +,QQ,AbstractSheaf)
    (symbol +,RingElement,AbstractSheaf)
  Headline
   (direct) sum of two abstract sheaves
  Usage
   F+G
   F++G
  Inputs
   F:
   G:
  Outputs
   :
    the (direct) sum of $F$ and $G$.  If one of the arguments is an integer, it represents the trivial sheaf of that rank.
  Description
   Example
    X = abstractProjectiveSpace 1
    OO_X(1) + OO_X(2)
    chi oo
    1 + OO_X(1)
    chi oo
   Text
    The sheaves can be on different varieties if one of the varieties is over the other.
   Example
    ch OO_X(1)
    Y = abstractProjectiveSpace'(3,X,VariableName=>k)
    ch OO_Y(2)
    OO_Y(2) ++ OO_X(1)
    ch oo
    chi ooo
--------
Node
  Key
   (symbol -,AbstractSheaf,AbstractSheaf)
   (symbol -,AbstractSheaf,QQ)
   (symbol -,AbstractSheaf,RingElement)
   (symbol -,AbstractSheaf,ZZ)
   (symbol -,QQ,AbstractSheaf)
   (symbol -,RingElement,AbstractSheaf)
   (symbol -,ZZ,AbstractSheaf)
  Headline
   difference of two abstract sheaves
  Usage
   F-G
  Inputs
   F:
   G:
  Outputs
   :
    the difference of $F$ and $G$.  If one of the arguments is an integer, it represents the trivial sheaf of that rank.
  Description
   Example
    X = abstractProjectiveSpace 1
    OO_X(1) - OO_X(2)
    chi oo
   Text
    The sheaves can be on different varieties if one of the varieties is over the other.
--------
Node
  Key
   (symbol ^,AbstractSheaf,ZZ)
  Headline
   power of an abstract sheaf
  Usage
   F^n
  Inputs
   F:
   n:
  Outputs
   :
    the {\tt n}-th Cartesian power of {\tt F}, which amounts to multiplication by {\tt n} in the Grothendieck group
  Description
   Text
    Maybe we should remove this operation, in favor of @ TO (symbol *, ZZ, AbstractSheaf) @.
   Example
    X = abstractProjectiveSpace 1
    F = OO_X(1)
    chi F^10
--------
Node
  Key
   (symbol ^**,AbstractSheaf,RingElement)
   (symbol ^**,AbstractSheaf,QQ)
   (symbol ^**,AbstractSheaf,ZZ)
  Headline
   tensor power of an abstract sheaf
  Usage
   F^**n
  Inputs
   F:
    of rank 1 if {\tt n} is not an integer
   n:
    in (or promotable to) the intersection ring of the variety of $F$
  Outputs
   :
    the $n$-th tensor power of $F$
  Description
   Example
    pt = base n
    X = abstractProjectiveSpace'(3,pt)
    A = intersectionRing X
    ch ((OO_X(1)) ^** 2)
    ch ((OO_X(1)) ^** 3)
    ch ((OO_X(1)) ^** n)
--------
Node
  Key
   (symbol SPACE,AbstractSheaf,RingElement)
   (symbol SPACE,AbstractSheaf,QQ)
   (symbol SPACE,AbstractSheaf,ZZ)
   (symbol SPACE,OO,RingElement)
  Headline
   twist by a divisor class
  Usage
   F(n)
  Inputs
   F:
   n:
    in (or promotable to) the intersection ring of the variety of $F$, homogeneous of degree 1 or 0
  Outputs
   :
    which depends on the degree of {\tt n}.
    In the case where {\tt n} has degree 0, the sheaf returned is the tensor product of {\tt F} with
    the {\tt n}-th (tensor) power of the tautological line bundle on the variety of {\tt F}.
    In the case where {\tt n} has degree 1, the sheaf returned is the tensor product of {\tt F} with
    the line bundle whose first Chern class is {\tt n}.
  Description
   Example
    X = abstractProjectiveSpace' 4
    OO_X(3)
    chi oo
    pt = base n
    Y = abstractProjectiveSpace'(4,pt)
    OO_Y(n)
    chi oo
   Text
    The notation {\tt OO(n)} is an abbreviation for {\tt OO_X(n)}, where {\tt X} is the variety whose intersection
    ring {\tt n} is in.  By default, the first Chern class of the tautological line bundle on a projective space
    or projective bundle is called {\tt h}, so we may use {\tt OO(h)} as alternative notation for {\tt OO_Y(1)}:
   Example
    A = intersectionRing Y
    chern OO_Y(1)
    OO(h)
    chern oo
  Caveat
   Beware the low parsing precedence of the adjacency operator @ TO symbol SPACE @.
  SeeAlso
   tautologicalLineBundle
--------
Node
  Key
   (Hom,AbstractSheaf,AbstractSheaf)
  Headline
   Hom between abstract sheaves
  SeeAlso
   dual
  Usage
   Hom(E,F)
  Inputs
   E:
   F:
  Outputs
   :
    the tensor product of $F$ with the dual of $E$
  Description
   Example
    X = abstractProjectiveSpace 1
    Hom(OO_X(3),OO_X)
    chi oo
--------
Node
 Key
  (symbol _,OO,RingElement)
 Headline
  the abstract structure sheaf of a divisor class
 Usage
  OO_D
 Inputs
  D:
   of degree 1, representing a divisor class on an abstract variety
 Outputs
  :
   the abstract sheaf $O_D$, defined as $1 - O(-D)$
 Description
  Example
   X = abstractProjectiveSpace' 4
   h
   F = OO_h
   chern F
   ch F
--------
Node
  Key
   (symbol _,OO,AbstractVariety)
  Headline
   the structure sheaf of an abstract variety
  Usage
   OO_X
  Inputs
   X:AbstractVariety
  Outputs
   :
    the structure sheaf of {\tt X}
  Description
   Example
    X = abstractProjectiveSpace 4
    OO_X
    rank oo
    chi ooo
--------
Node
  Key
   (symbol -,AbstractSheaf)
  Headline
   negation of an abstract sheaf
  Usage
   -F
  Inputs
   F:
  Outputs
   :
    the negation of {\tt F}, as a virtual sheaf.
  Description
   Example
    X = abstractProjectiveSpace 2
    - OO_X(1)
    chern oo
--------
Node
  Key
   (determinant,AbstractSheaf)
  SeeAlso
   (exteriorPower,ZZ,AbstractSheaf)
  Headline
   determinant of an abstract sheaf
  Usage
   det F
  Inputs
   F:
  Outputs
   :
    the determinant of {\tt F}
  Description
   Text
    The determinant of {\tt F} is the $n$-th exterior power of {\tt F}, where $n$ is the rank of {\tt F}.
   Example
    X = abstractProjectiveSpace 1
    F = OO_X(1) ++ OO_X(1) ++ OO_X(3)
    det F
    chern oo
    rank ooo
--------
Node
  Key
   (dual,AbstractSheaf)
  SeeAlso
   (Hom,AbstractSheaf,AbstractSheaf)
  Headline
   the dual of an abstract sheaf
  Usage
   dual F
  Inputs
   F:
  Outputs
   :
    the dual of {\tt F}
  Description
   Example
    X = abstractProjectiveSpace' 1
    dual OO_X(-1)
    chi oo
--------
Node
  Key
   (rank,AbstractSheaf)
  Usage
   rank F
  Inputs
   F:
  Outputs
   :
    the rank of {\tt F}
  Description
   Example
    rank tangentBundle abstractProjectiveSpace 4
--------
Node
  Key
   (variety,AbstractSheaf)
  Headline
   the variety of an abstract sheaf
  Usage
   variety F
  Inputs
   F:
  Outputs
   :
    the variety of {\tt F}
  Description
   Example
    X = abstractProjectiveSpace 4
    tangentBundle X
    variety oo
--------
Node
  Key
   (dim,AbstractVariety)
  Headline
   the dimension of an abstract variety
  Usage
   dim X
  Inputs
   X:
  Outputs
   :
    the dimension of {\tt X}
  Description
   Example
    dim abstractProjectiveSpace 4
--------
Node
  Key
   (use,AbstractVariety)
  Headline
   assign values to variables associated with a variety
  Usage
   use X
  Inputs
   X:
  Outputs
   :AbstractVariety
    {\tt X}
  Consequences
   Item
    The generators of the intersection ring of {\tt X} are assigned to their variables.  See @ TO (use,Ring) @.
   Item
    The named bundles on {\tt X} are assigned to their variables.
  Description
   Example
    S = base(n, Bundle => (E,3,c))
    E
    n
    n = E = 4;
    E
    n
    use S
    E
    n
--------
Node
  Key
   (symbol ^*,AbstractVarietyMap)
  Headline
   pull back operator associated to a map of varieties
  SeeAlso
   (symbol _*,AbstractVarietyMap)
  Usage
   f^*
  Inputs
   f:
  Outputs
   :
    the pull back operator associated to {\tt f}, a function that can be applied to an abstract sheaf or a cycle class
  Description
   Example
    S = base(4, Bundle => (E,4,c) )
    X = flagBundle( {2,2}, E )
    f = X.StructureMap
    chern E
    f^* oo
    f^* E
    chern oo
--------
Node
  Key
   (symbol _*,AbstractVarietyMap)
  Headline
   push forward operator associated to a map of varieties
  Usage
   f_*
  Inputs
   f:
  Outputs
   :
    the push forward operator associated to {\tt f}, a function that can be applied to an abstract sheaf or a cycle class
  SeeAlso
   integral
   (symbol ^*,AbstractVarietyMap)
  Description
   Example
    X = abstractProjectiveSpace' 4
    f = X.StructureMap
    OO_X(1)
    f_* oo
    ch OO_X(1)
    f_* oo
--------
Node
  Key
   (dim,AbstractVarietyMap)
  Headline
   the dimension of an abstract variety
  Usage
   dim X
  Inputs
   X:
  Outputs
   :
    the dimension {\tt X}
  Description
   Example
    dim abstractProjectiveSpace 4
--------
Node
  Key
   (source,AbstractVarietyMap)
  Headline
   the source of a map of abstract varieties
  Usage
   source f
  Inputs
   f:
  Outputs
   :
    the source of {\tt f}
  Description
   Example
    X = abstractProjectiveSpace 4
    f = X.StructureMap
    source f
--------
Node
  Key
   (target,AbstractVarietyMap)
  Headline
   the target of a map of abstract varieties
  Usage
   target f
  Inputs
   f:
  Outputs
   :
    the target of {\tt f}
  Description
   Example
    X = abstractProjectiveSpace 4
    f = X.StructureMap
    target f
--------
Node
  Key
   (symbol <-,ChernClassVariable)
  Headline
   assignment to a Chern class variable
  Usage
   v <- x
  Inputs
   v:
   x:
  Outputs
   :
    {\tt x}
  Consequences
   Item
    {\tt x} is assigned to the variable {\tt v}
  Description
   Text
    This intended for internal use by {\em Schubert2}.
   Example
    chern_3 E
    chern_3 E <- 444
    chern_3 E
--------
Node
  Key
   (value,ChernClassVariable)
  Headline
   the value of a Chern class symbol
  Usage
   value v
  Inputs
   v:
  Outputs
   :
    The value of {\tt v}.
  Description
   Example
    x = chern_3 E
    chern_3 E <- 444
    E
    x
    value x
  SeeAlso
   ChernClassVariableTable
--------
Node
  Key
   (chern,ZZ,Symbol)
  Headline
   make a Chern class symbol
  Usage
   chern(i,E)
   chern_i E
  Inputs
   i:
   E:
  Outputs
   :
    based on {\tt i} and on {\tt E}
  Description
   Example
    chern_3 E
-------
Node
  Key
   (symbol _,Ring,ChernClassVariable)
  Headline
   get a ring generator corresponding to a Chern class variable
  Usage
   R_v
  Inputs
   R:
   v:
  Outputs
   :
    the generator of {\tt R} corresponding to {\tt v}
  Description
   Example
    x = chern_3 E
    R = QQ[chern_1 E,chern_2 E,chern_3 E]
    R_x
-------
Node
 Key
  (sectionZeroLocus,AbstractSheaf)
  sectionZeroLocus
 Usage
  sectionZeroLocus F
 Inputs
  F:
 Outputs
  :
   the zero locus of a generic section of {\tt F}
 Description
  Example
   X = base(5, n, Bundle => (E,3,c), Bundle => (T,5,t), Bundle => (L,1,{h}))
   X.TangentBundle = T
   Y = sectionZeroLocus E
   Y.TautologicalLineBundle = OO_Y(h)
   chern tangentBundle Y
   integral oo
   chi ((tangentBundle Y)(n))
 Caveat
  The intersection ring provided for the zero locus contains only those classes arising by pull-back
  from the ambient variety: there is no algorithm to compute the intersection ring.
Node
 Key
  (degeneracyLocus2,ZZ,AbstractSheaf,AbstractSheaf)
  degeneracyLocus2
 Usage
  degeneracyLocus2(k,B,A)
 Inputs
  k:
  B:
  A:
 Outputs
  :
   the cycle class of the locus where the rank of a generic map from {\tt A} to {\tt B} is at most {\tt k}
 Description
  Example
   X = base(5, Bundle => (L,1,{l}), Bundle => (M,1,{m}), Bundle => (N,1,{n}))
   degeneracyLocus2(0,L+M+N,OO_X)
   degeneracyLocus2(1,L+M+N,2*OO_X)
   degeneracyLocus2(2,L+M+N,3*OO_X)
   X = base(5, Bundle => (A,3,a), Bundle => (B,3,b))
   degeneracyLocus2(0,B,OO_X)
   degeneracyLocus2(1,B,2*OO_X)
   degeneracyLocus2(2,B,3*OO_X)
   degeneracyLocus2(1,B,A)
   degeneracyLocus2(2,B,A)
   degeneracyLocus2(3,B,A)
Node
 Key
  (degeneracyLocus,ZZ,AbstractSheaf,AbstractSheaf)
  degeneracyLocus
 Usage
  degeneracyLocus(k,B,A)
 Inputs
  k:
  B:
  A:
 Outputs
  :
   the locus where the rank of a generic map from {\tt A} to {\tt B} is at most {\tt k}
 Description
  Example
   X = base(5, Bundle => (A,3,a), Bundle => (B,3,b))
   Z = degeneracyLocus(2,B,A)
   Z/X
   (Z/X)_* 1
Node
 Key
  (kernelBundle,ZZ,AbstractSheaf,AbstractSheaf)
  kernelBundle
 Usage
  kernelBundle(k,B,A)
 Inputs
  k:
  B:
  A:
 Outputs
  :
   the kernel bundle on the locus where the rank of a generic map from {\tt A} to {\tt B} is at most {\tt k}, assuming
   that the rank doesn't drop further
 Description
  Example
   X = base(5, Bundle => (A,3,a), Bundle => (B,3,b))
   E = kernelBundle(2,B,A)
   Z = variety E
   i = Z/X
   i_* 1
   i_* chern_1 E
   i_* (chern_1 E)^2
   i_* (chern_1 E)^3
   i_* E
   integral (chern_1 E)^4
   integral ch E
   F = kernelBundle(1,B,A)
   W = variety F
   j = W/X
   j_* chern_1 F
-------
Node
 Key
  (euler,AbstractVariety)
 Usage
  euler X
 Inputs
  X:
 Outputs
  :ZZ
   the topological Euler characteristic of the variety {\tt X}
 Description
  Example
   euler abstractProjectiveSpace 5
   euler flagBundle {1,1,1,1,1}
-------
Node
 Key
  "Examples from Schubert, translated"
 Description
  Text
   Here we present some examples from the Schubert documentation, translated line by line
   into {\em Schubert2} code.
 Subnodes
  :Examples from Schubert's documentation
  "Example from Schubert: Hilbert polynomial and Todd class of projective 3-space"
  "Example from Schubert: Generation of formulas"
  "Example from Schubert: Grassmannian of lines in P3"
  "Example from Schubert: Lines on a quintic threefold"
  "Example from Schubert: Conics on a quintic threefold"
  "Example from Schubert: Count the number of space conics intersecting 8 given lines"
  "Example from Schubert: Euler characteristic of Horrocks-Mumford bundle"
  "Example from Schubert: Riemann-Roch formulas"
  :Other examples
  "Example from Schubert: The number of elliptic cubics on a sextic 4-fold"
Node
 Key
  "Example from Schubert: Hilbert polynomial and Todd class of projective 3-space"
 Description
  Pre
   > proj(3,h,all): factor(chi(o(n*h)));
			     1/6 (n + 3) (n + 2) (n + 1)
  Example
   Ph = abstractProjectiveSpace'_3 base n
   factor chi OO(n*h)
  Pre
   > Ph[toddclass_];
					      2  2    3  3
			    1 + 2 h t + 11/6 h  t  + h  t
  Example
   todd Ph
Node
 Key
  "Example from Schubert: Generation of formulas"
 Description
  Pre
   > DIM:=4:
   > A:=bundle(2,c):        # a bundle with Chern classes c1,c2 and rank 2
   > B:=bundle(3,d):        # a bundle with Chern classes d1,d2,d3 and rank 3
  Example
   base(4, Bundle => (A,2,c), Bundle => (B,3,d))
  Pre
   > chern(A);
                                                  2
                                   1 + c1 t + c2 t
  Example
   chern A
  Pre
   > segre(B);
                                2        2      3                  3
                  1 + d1 t + (d1  - d2) t  + (d1  - 2 d1 d2 + d3) t
   
                            4          2               2   4
                       + (d1  - 3 d2 d1  + 2 d1 d3 + d2 ) t
  Example
   segre B
  Pre
   > chern(A&*B);           # The Chern class of the tensor product
                            2                 2                 2
   1 + (2 d1 + 3 c1) t + (d1  + 5 c1 d1 + 3 c1  + 2 d2 + 3 c2) t  +
   
                            3                           2                    2   3
     (6 c1 c2 + 2 d1 d2 + c1  + 2 d3 + 4 c1 d2 + 2 c1 d1  + 4 d1 c2 + 4 d1 c1 ) t
   
                                       2          2               2
      + (3 c1 d1 d2 + 6 d1 c1 c2 + 3 c2  + 3 c2 c1  + 2 d1 d3 + d2  + 3 c1 d3
   
               2       2        2   2        3   4
      + 2 c2 d1  + 3 c1  d2 + c1  d1  + d1 c1 ) t
  Example
   chern(A**B)
  Pre
   > chern(3,symm(3,dual(A)));
                                        3
                                  - 6 c1  - 30 c1 c2
  Example
   chern_3 symmetricPower_3 dual A
  Pre
   > segre(2,Hom(wedge(2,A),wedge(2,B)));
                                 2                 2
                             3 d1  - 8 c1 d1 + 6 c1  - d2
  Example
   segre_2 Hom(exteriorPower_2 A,exteriorPower_2 B)
Node
 Key
  "Example from Schubert: Grassmannian of lines in P3"
 Description
  Pre
   > grass(2,4,b,all): 
  Example
   Gb = flagBundle({2,2}, base n, VariableNames => {,b})
   Qb = last bundles Gb
  Pre
   > chi(Gb,Symm(n,Qb));
                                2            3
                               n  + 1 + 1/6 n  + 11/6 n
  Example
   chi symmetricPower_n Qb
  Pre
   > chi(Gb,o(n*b1));
                              4        3    23   2
                        1/12 n  + 2/3 n  + ---- n  + 7/3 n + 1
                                            12
  Example
   chi OO_Gb(n*b_1)
  Pre
   > 
   ## This should be a quadric in P5:
   > 
   > proj(5,H,all): chi(o(n*H)-o((n-2)*H));
                              4        3    23   2
                        1/12 n  + 2/3 n  + ---- n  + 7/3 n + 1
                                            12
  Example
   P5 = abstractProjectiveSpace'(5,base n,VariableName=>H)
   chi(OO(n*H)-OO((n-2)*H))
Node
 Key
  "Example from Schubert: Lines on a quintic threefold"
 Description
  Pre
   # Lines on a quintic threefold.  This is the top Chern class of the 
   # 5th symmetric power of the universal quotient bundle on the Grassmannian
   # of lines.
   > 
   > grass(2,5,c):        # Lines in P^4. 
  Example
   Gc = flagBundle({3,2}, VariableNames => {,c})
   (Sc,Qc) = bundles Gc
  Pre
   > B:=symm(5,Qc):       # Qc is the rank 2 quotient bundle, B its 5th 
   >                      # symmetric power.
  Example
   B = symmetricPower(5,Qc)
  Pre
   > c6:=chern(rank(B),B):# the 6th Chern class of this rank 6 bundle.
  Example
   c6 = chern(rank B,B)
  Pre
   > integral(c6);
                                         2875
  Example
   integral c6
Node
 Key
  "Example from Schubert: Conics on a quintic threefold"
 Description
  Pre
   # Conics on a quintic threefold. This is the top Chern class of the 
   # quotient of the 5th symmetric power of the universal quotient on the
   # Grassmannian of 2 planes in P^5 by the subbundle of quintic containing the 
   # tautological conic over the moduli space of conics.
   > 
   > grass(3,5,c):         # 2-planes in P^4.  
  Example
   Gc = flagBundle({2,3}, VariableNames => {,c})
   (Sc,Qc) = bundles Gc
  Pre
   > B:=Symm(2,Qc):        # The bundle of conics in the 2-plane. 
  Example
   B = symmetricPower(2,Qc)
  Pre
   > Proj(X,dual(B),z):    # X is the projective bundle of all conics. 
  Example
   X = projectiveBundle'(dual B, VariableNames => {,{z}})
  Pre
   > A:=Symm(5,Qc)-Symm(3,Qc)&*o(-z):  # The rank 11 bundle of quintics 
   >                                   # restricted to the universal conic. 
  Example
   A = symmetricPower_5 Qc - symmetricPower_3 Qc ** OO(-z)
  Pre
   > c11:=chern(rank(A),A):# its top Chern class.
  Example
   c11 = chern(rank A, A)
  Pre
   > lowerstar(X,c11):     # push down to G(3,5).
  Example
   X.StructureMap_* c11
  Pre
   > integral(Gc,");       # and integrate there.
--"
                                        609250
  Example
   integral oo
Node
 Key
  "Example from Schubert: Count the number of space conics intersecting 8 given lines"
 Description
  Pre
   > grass(3,4,d,all):
  Example
   Gd = flagBundle({1,3}, VariableNames => {,"d"})
   (Sd,Qd) = bundles Gd
  Pre
   > Proj(f,dual(symm(2,Qd)),e):
  Example
   f = projectiveBundle'(dual symmetricPower_2 Qd, VariableNames => {,{e}})
  Pre
   > integral(Gd,lowerstar(f,(2*d1+e)^8));
                                          92
  Example
   integral (2*d1 + e)^8
Node
 Key
  "Example from Schubert: Euler characteristic of Horrocks-Mumford bundle"
 Description
  Pre
   > proj(4,h,tang):            # need tangentbundle for chi
  Example
   X = abstractProjectiveSpace'(4,base n,VariableName => h)
  Pre
   > F:=sheaf(2,[5*h,10*h^2]):  # defines the Horrocks-Mumford bundle
  Example
   F = abstractSheaf(X, Rank => 2, ChernClass => 1 + 5*h + 10*h^2)
  Pre
   > chi(F&*o(n*h));            # computes chi of its twists
                             4        3   125  2
                       1/12 n  + 5/3 n  + --- n  + 125/6 n + 2
                                           12
  Example
   chi (F ** OO(n*h))
Node
 Key
  "Example from Schubert: Riemann-Roch formulas"
 Description
  Pre
   # Line bundle O(D) on a threefold.
   > 
   > variety(X,dim=3,tan=sheaf(3,[-K,c2,c3])): # traditionally, -K is 
   >                                           # used instead of c1
  Example
   X = abstractVariety(3,QQ[K,c_2,c_3, Degrees => {1..3}][D,Join=>false])
   X.TangentBundle = abstractSheaf(X,Rank=>3,ChernClass=>1-K+c_2+c_3)
  Pre
   > chi(o(D));
                           3          2          2
          integral(X, 1/6 D  - 1/4 K D  + (1/12 K  + 1/12 c2) D - 1/24 K c2)
  Example
   chi OO(D)
Node
 Key
  "Example from Schubert: The number of elliptic cubics on a sextic 4-fold"
 Description
  Text
   We thank Rahul Pandharipande for this classic Schubert example.  See
   @ HREF{ "http://arxiv.org/abs/math/0702189", "Enumerative Geometry of Calabi-Yau 4-Folds"}  @.
  Pre
   > grass(3,6,c):
  Example
   Gc = flagBundle({3,3}, VariableNames => {,c})
   (Sc,Qc) = bundles Gc
  Pre
   > B:=Symm(3,Qc):
  Example
   B = symmetricPower_3 Qc
  Pre
   > Proj(X,dual(B),z):
  Example
   X = projectiveBundle'(dual B, VariableNames => {,{z}})
  Pre
   > A:=Symm(6,Qc)-Symm(3,Qc)&@o(-z):
  Example
   A = symmetricPower_6 Qc - symmetricPower_3 Qc ** OO(-z)
  Pre
   > c18:=chern(rank(A),A):
   > lowerstar(X,c18):
   > integral(Gc,%);
   Ans =  2734099200
  Example
   integral chern A
-------
Node
 Key
  "Riemann-Roch on a curve"
 Description
  Text
   We follow Example 15.2.1 of Fulton's book, {\em Intersection Theory}.
  Example
    X = abstractVariety(1,QQ[r,s,e_1,f_1,D,K,Degrees=>{2:0,4:1}])
    X.TangentBundle = abstractSheaf(X,Rank=>1,ChernClass=>1-K)
    chi OO_X
    chi OO(D)
    E = abstractSheaf(X,Rank => r, ChernClass => 1+e_1)
    F = abstractSheaf(X,Rank => s, ChernClass => 1+f_1)
    chi Hom(E,F)
-------
Node
 Key
  "Riemann-Roch on a surface"
 Description
  Text
   We follow Example 15.2.2 of Fulton's book, {\em Intersection Theory}.
  Example
    X = abstractVariety(2,QQ[r,D,d_1,K,c_2,d_2,Degrees=>{0,3:1,2:2}])
    X.TangentBundle = abstractSheaf(X,Rank=>2,ChernClass=>1-K+c_2)
    todd X
    chi OO_X
    E = abstractSheaf(X,Rank => r, ChernClass => 1+d_1+d_2)
    chi ( E - rank E * OO_X )
    chi ( OO(D) - OO_X )
    chi OO_D
  Text
    We define a function to compute the arithmetic genus and use it to compute
    the arithmetic genus of a curve on $X$ whose divisor class is $D$:
  Example
    p_a = D -> 1 - chi OO_D;
    p_a D
  Text
    We we compute the arithmetic genus of a curve of degree $n$ in $\PP^2$:
  Example
    Y = abstractProjectiveSpace'_2 base n
    factor p_a (n*h)
  Text
   Here we compute the arithmetic genus of a curve on with $\PP^1 \times \PP^1$:
  Example
    Z = abstractProjectiveSpace'_(1,VariableName => k) abstractProjectiveSpace'_1 base(m,n)
    factor p_a (m*h + n*k)
  Text
   In the code above we have used the notation {\tt f_(a,b) x} as an abbreviation for {\tt f(a,b,x)}, see @ TO (symbol _,Function,Thing) @.
-------
Node
 Key
  "Riemann-Roch without denominators"
 Description
  Text
   We display some of the universal polynomials described in Lemma 15.3 of Fulton's book, {\em Intersection Theory}.
  Example
    f = (n,d,e) -> (
	X = base(n, 
	     Bundle => (symbol D,d,chern_1 symbol D .. chern_d symbol D),
	     Bundle => (symbol E,e,chern_1 symbol E .. chern_e symbol E));
	p := chern(exteriorPower dual D * E) - 1;
	assert( p % ctop D == 0 );
	p // ctop D );
    n = 4;
    for d from 1 to 3 do for e from 1 to 4 do << endl << "d=" << d << " e=" << e << " P(D,E) = " << f(n,d,e) << endl
-------
Node
 Key
  (map,FlagBundle,AbstractVarietyMap,List)
  (map,FlagBundle,AbstractVariety,List)
 Headline
  make a map from an abstract variety to a flag bundle
 Usage
  map(F,f,b)
  map(F,X,b)
 Inputs
  F:
   over a variety {\tt S}, of the form $flagBundle(\{r_0, ..., r_{n-1}\},E)$, say, where $E$ is an abstract sheaf on $S$
  :
   {\tt f}, @ ofClass AbstractVarietyMap @ from a variety $X$, say, to $S$;
   or
   {\tt X}, @ ofClass AbstractVariety @, with a structure map $f : X \rightarrow{} S$
  b:
   of the form $\{B_0,...B_{n-1}\}$ whose elements are abstract sheaves on {\tt X}, with $rank B_i = r_i$, for each i.
   The sheaves should be {\em effective} in the sense that $c_j B_i = 0$ for $j > r_i$.
   The sum of the sheaves should equal $f^* E$; alternatively, one of them can be omitted and it will be deduced from
   the condition on the sum.
  Degree => Nothing
   the value of this option is ignored
  DegreeLift => Nothing
   the value of this option is ignored
  DegreeMap => Nothing
   the value of this option is ignored
 Outputs
  :AbstractVarietyMap
   the map of abstract varieties $g : X \rightarrow{} F$ over $S$ such that $g^* (E_{i+1}/E_i) = B_i$, for each i, where 
   $0 = E_0 \subseteq{} E_1 \subseteq{} ... \subseteq{} E_n = p^* E$ is the tautological filtration on $F$, 
   and where $p : F \rightarrow{} S$ is the structure map of $F$.
 Description
  Text
   Not implemented yet.
-------
Node
 Key
  (symbol /,AbstractVariety,AbstractVariety)
 Headline
  get a structure map
 Usage
  X/S
 Inputs
  X:
  S:
 Outputs
  :
   the structure map or composition of structure maps $X \rightarrow{} S$
 SeeAlso
  StructureMap
 Description
  Example
   X = abstractProjectiveSpace_3 point
   Y = abstractProjectiveSpace_2 X
   Z = abstractProjectiveSpace_1 Y
   Z/Z
   Z/Y
   Z/X
   Z/point
--
Node
 Key
  (id, AbstractVariety)
 Headline
  the identity map of an abstract variety
 Usage
  id_X
 Inputs
  X:
 Outputs
  :
   the identity map from {\tt X} to itself
 Description
  Example
   X = abstractProjectiveSpace 4
   id_X
---
Node
 Key
  (symbol *,AbstractVarietyMap,AbstractVarietyMap)
 Headline
  composition of maps of abstract varieties
 Usage
  g*f
 Inputs
  g:
  f:
 Outputs
  :
   the compostion of {\tt g} and {\tt f}
 Description
  Example
   X = abstractProjectiveSpace_3 point
   Y = abstractProjectiveSpace_4 X
   Y.StructureMap
   X.StructureMap * Y.StructureMap
Node
 Key
  ChernClassVariableTable
 Headline
  the class of all Chern class variable tables
 Description
  Text
   After creating a Chern class variable such as @ TT "chern_3 E" @, the base symbol {\tt E} acquires as value 
   a hash table that holds the values of the Chern class variables based on it.  If something else is assigned
   to {\tt E}, the values are no longer referred to by {\tt E}, and will the space they occupy be collected
   and reused unless other references to them remain.
  Example
   chern_3 E
   E
   peek E
   chern_3 symbol E <- 44
   chern_3 E
   peek E
   E = 55
   chern_3 symbol E <- 44
Node
 Key
  (chern,ZZ,ChernClassVariableTable)
 Headline
  get value of a Chern class variable
 Usage
  chern_i E
 Inputs
  i:
  E:
 Outputs
  :
   the value of the Chern class variable @ TT "chern_i E" @
 Description
  Example
   chern_3 E <- 44
   E
   chern_3 E
Node
  Key
    (map,FlagBundle,FlagBundle)
  Headline
    forgetful maps of flag varieties
  Usage
    map(X,Y)
  Inputs
    X:FlagBundle
      with ranks ${a_1,..,a_n}$ of some bundle $E$
    Y:FlagBundle
      with ranks ${b_1,..,b_k}$ of the same bundle $E$, and such that there are indices
      $0 = i_0 < i_1 < \ldots < i_n = k$ such that for all $l$ from $1$ to $n$ we have
      $b_(i_(l-1)+1) + \ldots + b_(i_l) = a_l$.
  Outputs
    :AbstractVarietyMap
      the forgetful map from $Y$ to $X$
  Description
    Example
      F1 = flagBundle({1,2,2,1,3})
      F2 = flagBundle({3,3,3})
      f = map(F2,F1)
Node
  Key
    (map,FlagBundle,AbstractVariety,AbstractSheaf)
  Headline
    maps to projective bundles
  Usage
    map(P,X,L)
  Inputs
    P:FlagBundle
      the projectivization of some vector bundle E
    X:AbstractVariety
    L:AbstractSheaf
      a line bundle on X, the desired pullback of $O_P(1)$.
  Outputs
    :AbstractVarietyMap
      the map from X to P such that the pullback of $O_P(1)$ is $L$
  Description
    Text
      Accepts both Grothendieck-style and Fulton-style ${\mathbb P}(E)$, but in the case a 
      decision cannot be made based on ranks (i.e. when $E$ has rank $2$), defaults to
      Fulton-style notation (so ${\mathbb P}(E)$ is the space of sub-line-bundles of $E$).
      
      Does not check whether $L$ is basepoint-free.  Weird results are probably possible if $L$
      is not.
    Example
      X = flagBundle({2,2}) --the Grassmannian GG(1,3)
      (S,Q) = bundles X
      L = exteriorPower(2,dual S)
      P = flagBundle({5,1}) --Grothendieck-style PP^5
      f = map(P,X,L) -- Plücker embedding of GG(1,3) in PP^5
      H = last bundles P
      f^* (chern(1,H)) -- hyperplane section, should be sigma_1
      f_* chern(0,S) --expect 2 times hyperplane class since GG(1,3) has degree 2
 
Node
  Key
    toSchubertBasis
    (toSchubertBasis,RingElement)
  Headline
    express cycles on a Grassmannian in terms of the Schubert basis
  Usage
    toSchubertBasis c
  Inputs
    c:RingElement
      An element of the intersection ring of a Grassmannian of $k$-dimensional subspaces of a
      rank-$n$ vector bundle
  Outputs
    :
      An element $c'$ of a polynomial ring $B[s_\lambda]$ where $B$ is the base ring of G and
      $\lambda$ runs over all diagrams in a $k\times n$ rectangle (this is the "Schubert ring" of G,
      see @TO schubertRing@).  The element $c'$ is the
      representation of $c$ in terms of the Schubert basis of the intersection ring of G over B.
  Description
    Text
      Note that the Schubert basis used here is in "Fulton-style" notation; see
      @TO schubertCycle@.
    Example
      A = flagBundle({3,3},VariableNames => H)
      S = first bundles A
      G = flagBundle({1,2},S,VariableNames => K)
      RG = intersectionRing G
      c = H_(2,3)*((K_(2,1))^2) + H_(1,1)*K_(2,2)
      toSchubertBasis c
  SeeAlso
    schubertRing
Node
  Key
    incidenceCorrespondence
    (incidenceCorrespondence, FlagBundle, FlagBundle)
  Headline
    build containment correspondence between two Grassmannians
  Usage
    incidenceCorrespondence(G2,G1)
  Inputs
    G2:FlagBundle
      a Grassmannian of $b$-dimensional subbundles of a vector bundle $E$
    G1:FlagBundle
      another Grassmannian of $a$-dimensional subbundles of the same bundle $E$
  Outputs
    :IncidenceCorrespondence
      from $G1$ to $G2$, namely the correspondence whose
      intermediate term is flagBundle({a,b-a,n-b},E) (in the case that $a<=b$).  This
      intermediate term can be viewed as those pairs of subbundles $(V_1,V_2)$ of $E$ of
      ranks $a$ and $b$, respectively, such that $V_1 \subseteq V_2$.
  Description
    Example
      P3 = flagBundle({1,3},VariableNames => H)
      GG13 = flagBundle({2,2},VariableNames => K)
      I = incidenceCorrespondence(GG13,P3)
      c = chern(1, last bundles P3) --the hyperplane class
      I_* (c^2) --the class in GG13 corresponding to lines meeting a given line, i.e. sigma_1
      d = (chern(2, last bundles GG13))^2 --the class of a point in GG13
      I^* d --the class in $P3$ of points lying in the line corresponding to d, i.e. c^2
Node
  Key
    (incidenceCorrespondence,List,AbstractSheaf)
  Headline
    build containment correspondence between two Grassmannians
  Usage
    incidenceCorrespondence(L,E)
  Inputs
    L:List
      of two integers, $L={a,b}$, such that $0 \leq a$ and $0\leq b$.
    E:AbstractSheaf
      a vector bundle of rank $n$, such that $a\leq n$ and $b\leq n$.
  Outputs
    :IncidenceCorrespondence
      from $G1 = G(a,E)$ to $G2 = G(b,E)$, same as {\tt incidenceCorrespondence(G1,G2)}
  Description
    Example
      B = flagBundle({3,1})
      E = first bundles B
      incidenceCorrespondence({1,2},E)
  SeeAlso
    (incidenceCorrespondence,FlagBundle,FlagBundle)
    (incidenceCorrespondence,List)
Node
  Key
    (incidenceCorrespondence,List)
  Headline
    build containment correspondence between two Grassmannians
  Usage
    incidenceCorrespondence L
  Inputs
    L:List
      of three integers, $L={a,b,n}$, such that $0 \leq a \leq n$ and $0\leq b \leq n$.
  Outputs
    :IncidenceCorrespondence
      from $G1 = G(a,n)$ to $G2 = G(b,n)$, same as {\tt incidenceCorrespondence(G1,G2)}
  Description
    Example
      I = incidenceCorrespondence({1,2,3})
      source I
      target I
  SeeAlso
    (incidenceCorrespondence,FlagBundle,FlagBundle)
    (incidenceCorrespondence,List,AbstractSheaf)
Node
  Key
    IncidenceCorrespondence
  Headline
    the class of all incidence correspondences
  Description
    Text
      An incidence correspondence $I$ is a correspondence from an @TO AbstractVariety@ $X$ 
      (the @TO source@ of $I$) to another AbstractVariety $Y$ (the @TO target@ of $I$)
      which is mediated by a third AbstractVariety $Z$, together with 
      @TO AbstractVarietyMap@s $f:Z\rightarrow X$ and $g:Z\rightarrow Y$.
      
      Given a cycle $x$ on $X$, the pushforward of $x$ along $I$ is
      $$I_* x = g_* f^* x,$$ 
      and similarly for bundles on $X$.  Likewise, if $y$ is a cycle on $Y$, the pullback
      of $y$ along $I$ is
      $$I^* y = f_* g^* y,$$
      and similarly for bundles on $Y$.  See @TO (symbol ^*,Correspondence)@ and
      @TO (symbol _*,Correspondence)@ for more info.
      
      The tuple $(Z,f,g)$ can be accessed via the @TO intermediates@ command.
Node
  Key
    Correspondence
  Headline
    the class of all correspondences
  Description
    Text
      A correspondence $I$ from from an @TO AbstractVariety@ $X$ 
      (the @TO source@ of $I$) to another AbstractVariety $Y$ (the @TO target@ of $I$)
      consists of two functions $I_*$ and $I^*$, which take cycles on $X$ to cycles on $Y$
      and vice-versa, respectively.  In general these functions are not ring maps or even
      module maps.  See @TO (symbol ^*,Correspondence)@ and
      @TO (symbol _*,Correspondence)@ for more info.
      
      Unlike an @TO IncidenceCorrespondence@, a general Correspondence need not have an
      intermediate variety.
Node
  Key
    (transpose,Correspondence)
    (transpose,IncidenceCorrespondence)
  Headline
    reverse the direction of a correspondence
  Usage
    transpose I
  Inputs
    I:Correspondence
      from $X$ to $Y$
  Outputs
    J:Correspondence
      from $Y$ to $X$, such that $J_* = I^*$ and $J^* = I_*$.  In the case of an 
      @TO IncidenceCorrespondence@, the @TO intermediates@ are also carried along appropriately,
      that is, if {\tt intermediates I} is the tuple $(Z,f,g)$, then {\tt intermediates J}
      is the tuple $(Z,g,f)$.
Node
  Key
    (symbol *, Correspondence, Correspondence)
  Headline
    composition of correspondences
  Usage
    J * I
  Inputs
    I:Correspondence
      from $X$ to $Y$
    J:Correspondence
      from $Y$ to $Z$
  Outputs
    K:Correspondence
      from $X$ to $Z$, such that $K_* = J_* I_*$ and $K^* = I^* J^*$
Node
  Key
    (symbol ^*, Correspondence)
  Headline
    pullback along a correspondence
  Usage
    I^*
  Inputs
    I:
      from $X$ to $Y$
  Outputs
    :
      the pullback operator associated to $I$, which can be applied to abstract sheaves or cycles
      on $Y$
  Description
    Text
      See @TO incidenceCorrespondence@ for an example of the usage.
Node
  Key
    (symbol _*, Correspondence)
  Headline
    pushforward along a correspondence
  Usage
    I_*
  Inputs
    I:
      from $X$ to $Y$
  Outputs
    :
      the pushforward operator associated to $I$, which can be applied to abstract sheaves or cycles
      on $X$
  Description
    Text
      See @TO incidenceCorrespondence@ for an example of the usage.
Node
  Key
    (source,Correspondence)
  Headline
    the source of a correspondence
  SeeAlso
    Correspondence
Node
  Key
    (target,Correspondence)
  Headline
    the target of a correspondence
  SeeAlso
    Correspondence
Node
  Key
    intermediates
    (intermediates, IncidenceCorrespondence)
  Usage
    intermediates I
  Inputs
    I:IncidenceCorrespondence
  Outputs
    :
      a triple $(Z,f,g)$ where $Z$ is the AbstractVariety mediating the Correspondence, $f$ is
      the AbstractVarietyMap from $Z$ to the source of $I$, and $g$ is the AbstractVarietyMap
      from $Z$ to the target of $I$.
Node
  Key
    bundles
    (bundles,FlagBundle)
  Headline
    get the list of tautological line bundles on a flag bundle
  Usage
    bundles X
  Inputs
    X:FlagBundle
  Outputs
    :List
      of bundles, the tautological bundles on the flagBundle X
  Description
    Example
      X = flagBundle({2,3,4})
      L = bundles X
      rank \ L
Node
  Key
    tautologicalLineBundle
    (tautologicalLineBundle,AbstractVariety)
    (tautologicalLineBundle,FlagBundle)
  Headline
    get the tautological line bundle of a flag bundle or abstract variety
  Usage
    tautologicalLineBundle X
  Description
    Text
      Returns the tautological line bundle of the variety $X$.  In the case of a
      @TO FlagBundle@, this is the line bundle defining the Pl\"ucker embedding.
      
      Used in @TO (symbol SPACE,AbstractSheaf,ZZ)@.
  SeeAlso
    (symbol SPACE,AbstractSheaf,ZZ)
Node
  Key
    SchubertRing
  Headline
    a symbol used internally as a key
  Description
    Text
      If $G$ is a Grassmannian (i.e. a @TO FlagBundle@ with rank sequence of length 2), and if
      the Schubert ring of $G$ has already been built, then it is stored in 
      {\tt G.cache.SchubertRing}.
      This should not be used as an accessor; see @TO schubertRing@.
  SeeAlso
    schubertRing
Node
  Key
    schubertRing
    (schubertRing,FlagBundle)
  Headline
    get the Schubert ring of a Grassmannian
  Usage
    schubertRing G
  Inputs
    G:FlagBundle
      a Grassmannian, with intersection ring $R$, say
  Outputs
    :
      A triple $(S,T,U)$, where $S$ is the Schubert ring of $G$,
      with generators corresponding to the (Fulton-style) Schubert
      cycles of $G$ and with multiplication corresponding to multiplication in the intersection
      ring of $G$, $T$ is the map from $R$ to $S$ converting from the Chern class basis to
      the Schubert basis, and $U$ is the inverse map to $T$.
  Consequences
    Item
      When run for the first time on $G$, the Schubert ring $S$ is built and cached in
      {\tt G.cache.SchubertRing}, see @TO SchubertRing@, $T$ is built and cached in
      {\tt G.cache.htoschubert}, and $U$ is built and cached in {\tt G.cache.schuberttoh}.
  Description
    Example
      G = flagBundle({2,2})
      (S,T,U) = schubertRing G
      c = schubertCycle({1,0},G)
      a = T c
      a^2
      U oo
  SeeAlso
    toSchubertBasis
-- Local Variables:
-- mode: M2
-- coding: utf-8
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Schubert2 "
-- End:
