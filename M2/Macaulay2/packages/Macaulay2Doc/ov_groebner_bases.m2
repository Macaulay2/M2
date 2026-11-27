-- -*- coding: utf-8 -*-
--- status: DRAFT
--- author(s): kummini
--- notes:

doc ///
  Key
    "Gröbner bases"
  Headline
    an overview of Gröbner basis computations
  Description
    Text
      A Gröbner basis is a specific generating set for an ideal or submodule,
      often used to compute other information, such as numerical invariants,
      operations such as intersections and ideal quotients, syzygies, etc. In
      Macaulay2, Gröbner bases are computed behind the scenes when needed, and
      the Gröbner basis is cached in case it is needed later.

      See @TO GroebnerBasis@ for additional common operations and a
      comprehensive list of all routines in Macaulay2 which either take Gröbner
      bases as arguments, or return one.

      Common Gröbner-basis tasks are routed as follows.

      @UL {
          {"To compute a Gröbner basis and extract its generators, use ", TO gb, " and ", TO generators, "."},
          {"To inspect leading terms or coefficients, use ", TO leadTerm, ", ", TO leadMonomial, ", and ", TO leadCoefficient, "."},
          {"To reduce modulo an ideal or test normal forms, see ", TO "normal forms", " and ", TO (symbol %, RingElement, Ideal), "."},
          {"To eliminate variables or find implicit equations, see ", TO "elimination of variables", ", ", TO "Elimination::eliminate", ", ", TO selectInSubring, ", and ", TO (kernel, RingMap), "."},
          {"To control a slow or partial computation, see ", TO "fine control of a Groebner basis computation", ", ", TO gbSnapshot, ", ", TO "gbTrace", ", and ", TO forceGB, "."}
          }@

      In Macaulay2, Gröbner bases may be computed for ideals and submodules
      over the following rings:

      @UL {
          {TO ZZ, " -- Hermite normal form"},
          "fields -- Gaussian elimination",
          "polynomial rings over a field or over the integers (including skew commutative multiplication)",
          "quotients of such rings",
          "Weyl algebras"
          }@

      Gröbner bases of ideals in polynomial rings over other polynomial rings
      are also allowed.
  Subnodes
    "what is a Groebner basis?"
    "Groebner basis examples and applications"
    "fine control of a Groebner basis computation"
    "computing Groebner bases"
    -- Mike wanted this: TO "partial computation of a Groebner basis",
    -- Mike wanted this: TO "Hilbert driven Groebner basis",
    --TO "finding a Groebner basis",
    --TO "rings that are available for Groebner basis computations",
///

doc ///
  Key
    "Groebner basis examples and applications"
  Subnodes
    "simple Groebner basis computations over various rings"
    -- Mike wanted this: TO "Groebner bases in local rings",
    -- this is already a subnode somewhere else: TO "normal forms",
    -- this is already a subnode somewhere else: TO "elimination of variables",
    -- Mike wanted this: TO "saturation"
///

doc ///
  Key
    "simple Groebner basis computations over various rings"
  Description
    Text
      For a definition of Gröbner basis, see @TO "what is a Groebner basis?"@.

      @SUBSECTION "A first Gröbner basis"@

      @SUBSECTION "Weight vectors and non-strict monomial orders"@

      The monomial order may be given by a weight vector (see
      @TO "monomial orderings"@),
    Example
      R = QQ[a..f,MonomialOrder=>Weights=>{1,1,1,1,0,0}]
      I = ideal(a*b*c-d*e*f,a*c*e-b*d*f,a*d*f-b*c*e)
      gens gb I
      leadTerm I
    Text
      The monomial order used here is: first consider the dot product of the
      weight vector and the exponent vectors. The larger determines the lead
      term. If these dot products are the same, then ties are broken with
      graded reverse lexicographic order. Sometimes, one wants the lead term
      ideal with respect to the weight vector itself. This ideal, often not a
      monomial ideal, may be obtained by selecting only the first part of the
      monomial order while finding the lead term matrix:
    Example
      leadTerm(1,I)
    Text
      @SUBSECTION "Gröbner basis over the integers"@

      A strong Gröbner basis of an ideal $I \subset{} ZZ[x_1,...,x_n]$ is a set
      $G$ of elements of $I$ such that, if $cm$ is any lead term of $I$ ($c$ a
      coefficient, and $m$ a monomial), then there is an element of $G$ whose
      lead term divides $cm$.

      Macaulay2 computes such strong Gröbner bases over ZZ.
    Example
      R = ZZ[x,y]
      F = y^2-(x^3+3*x+5)
      I = ideal(F, diff(x,F), diff(y,F))
      gens gb I
      leadTerm I
      factor 174
    Text
      This elliptic curve is smooth for all primes $p \neq 2, 3, 29$.

      @SUBSECTION "Gröbner basis over a quotient ring"@

      The lead terms of the Gröbner basis generate all possible lead terms
      modulo the lead terms of the quotient ideal.
    Example
      R = QQ[a..d]/(a^2+b^2+c^2+d^2-1)
      I = ideal(a*b*c*d)
      gens gb I
    Text
      @SUBSECTION "A Gröbner basis over a skew-commutative ring"@

      The lead terms of the Gröbner basis generate all possible lead terms, as
      in the commutative case. Essentially the same algorithm is applicable in
      this case.
    Example
      R = QQ[a..d,SkewCommutative=>true]
      I = ideal(a*b-c*d)
      gens gb I
    Text
      @SUBSECTION "Gröbner basis over polynomial rings over polynomial rings"@

      For such rings, we may imagine a polynomial ring over a base field, or
      over the integers, possibly modulo some ideal. The resulting Gröbner
      basis is this one. The monomial order is a product order, with the
      variables in the outermost polynomial ring the most expensive.
    Example
      A = QQ[s,c]/(s^2+c^2-1)
      B = A[x,y,z]
      I = ideal(c*x^2, s*y^2, c*y-s*x)
      gens gb I
      leadTerm oo
    Text
      @SUBSECTION "A Gröbner basis of a submodule"@
///

doc ///
  Key
    "normal forms"
  Headline
    compute normal forms with Gröbner bases
  Description
    Text
      Use normal forms when you want to reduce an element modulo an ideal,
      compare two expressions modulo an ideal, or test membership by checking
      whether the remainder is zero. In Macaulay2 the operator
      @TO (symbol %, RingElement, Ideal)@ computes this remainder using a
      Gröbner basis.

      Let $R = k[x_1, ..., x_n]$ be a polynomial ring over a field k, and let
      $I \subset{} R$ be an ideal. Let $\{g_1, ..., g_t\}$ be a Gröbner basis
      for $I$. For any $f \in{} R$, there is a unique `remainder' $r \in{} R$
      such that no term of $r$ is divisible by the leading term of any $g_i$
      and such that $f-r$ belongs to $I$. This polynomial $r$ is sometimes
      called the normal form of $f$.

      For an example, consider symmetric polynomials. The normal form of the
      symmetric polynomial @TT "f"@ with respect to the ideal @TT "I"@ below
      writes @TT "f"@ in terms of the elementary symmetric functions
      @TT "a,b,c"@.
    Example
      R = QQ[x,y,z,a,b,c,MonomialOrder=>Eliminate 3];
      I = ideal(a-(x+y+z), b-(x*y+x*z+y*z), c-x*y*z)
      f = x^3+y^3+z^3
      f % I
  SeeAlso
    "Gröbner bases"
    (symbol %, RingElement, Ideal)
  Subnodes
    "methods for normal forms and remainder"
///

-- we should be able to link to the right nodes without this:
-- needsPackage "Elimination"
doc ///
  Key
    "elimination of variables"
  Headline
    eliminate variables with Gröbner bases
  Description
    Text
      Use elimination when you want equations involving only selected
      variables, for example to eliminate parameters from a parametrization.
      The main approaches are to use an elimination monomial order with
      @TO selectInSubring@, call @TO "Elimination::eliminate"@, or construct a
      ring map and compute its @TO (kernel, RingMap)@.

      Let's consider the problem of finding the polynomial relation of the
      three polynomials $x = s^3+st+1$, $y = t^3+3t^2+t$, and $z = st^3$. The
      first method we use is to compute a Gröbner basis using an elimination
      order which eliminates the variables s,t, and then select those Gröbner
      basis elements that do not involve the variables s and t.
    Example
      R = QQ[s,t,x,y,z, MonomialOrder=>Eliminate 2];
      I = ideal(x-s^3-s*t-1, y-t^3-3*t^2-t, z-s*t^3)
      time leadTerm gens gb I
      G = selectInSubring(1,gens gb I)
      ans1 = G_(0,0)
    Text
      This method (with some optimizations incorporated) is provided by the
      @TO "Elimination::eliminate"@ function.
    Example
      R = QQ[x,y,z,s,t];
      I = ideal(x-s^3-s*t-1, y-t^3-3*t^2-t, z-s*t^3)
      time G = eliminate(I,{s,t})
      ans2 = G_0
    Text
      Sometimes giving the variables different degrees will speed up the
      computations. Here, we set the degrees of x, y, and z to be the total
      degrees.
    Example
      R1 = QQ[x,y,z,s,t, Degrees=>{3,3,4,1,1}];
      I1 = substitute(I,R1);
      time G = eliminate(I1,{s,t})
      ans3 = G_0
    Text
      Another approach is to create the ring map $F : k[x,y,z] \rightarrow{}
      k[s,t]$, and find its kernel.
    Example
      A = QQ[s,t];
      B = QQ[x,y,z];
      F = map(A,B,{s^3+s*t+1, t^3+3*t^2+t, s*t^3})
      time G = kernel F
      ans4 = G_0
    Text
      This appears to be much faster than the first two methods.

      Finally, we may use resultants to find elements of the ideal I which do
      not involve the variables s and t.
    Example
      use ring I
      time f1 = resultant(I_0,I_2,s)
      time f2 = resultant(I_1,f1,t)
      ans5 = -f2
    Text
      This is the fastest method in this case.

      These answers should all be the same (with the possible exception of the
      last), but are they? They live in different rings, so we cannot compare
      them directly. Instead, let's move them to the ring B, and then remove
      duplicates.
    Example
      L = {ans1,ans2,ans3,ans4,ans5};
      L = apply(L, f -> substitute(f,B));
      length unique L
    Text
      They are all the same!
  SeeAlso
    selectInSubring
    RingMap
    "Elimination::eliminate"
    "Elimination::resultant"
    apply
    unique
    length
///
doc ///
  Key
    "what is a Groebner basis?"
  Description
    Text
      A Gröbner basis is a specific generating set of an ideal or submodule
      over a polynomial ring. It is not minimal in general, but has extremely
      nice properties; it is reasonably easy to extract information about the
      ideal or submodule from a Gröbner basis. We first describe Gröbner bases
      in the important special case of an ideal in a polynomial ring. We will
      then describe Gröbner bases of submodules, and over more general rings.

      For computation, use @TO gb@; for the basis elements, use @TO generators@
      on the resulting Gröbner basis. Normal forms and elimination are common
      applications of the same computation.

      Let $R = k[x_1, ..., x_n]$ be a polynomial ring over a field $k$, and let
      $I \subset R$ be an ideal. A {\it monomial order} on $R$ is a total
      order, $>$, on the monomials of $R$, which satisfies two conditions: (1)
      $m > 1$, for every monomial $m \neq 1$, and (2) the order is
      multiplicative: $m > n$ implies that $mp > np$, for all monomials $m$,
      $n$, $p$.

      In Macaulay2, each ring has a monomial order (also called a term order)
      associated with it. The default monomial order is @TT "GRevLex"@. See
      @TO "monomial orderings"@ for more information.

      Given a term order, the leading term is the one whose monomial is
      greatest in this order.
    Example
      R = ZZ/1277[a..d]
      F = 5*a^3 + d^2 + a*d + b*c + 1
      leadTerm F
    Text
      For an ideal $I \subset R$, the initial ideal $in(I)$ is the ideal
      generated by the leading terms of the elements in I. A Gröbner basis for
      I is a set of generators for I whose leading terms generate $in(I)$.
    Example
      R = ZZ/1277[x,y];
      I = ideal(x^3 - 2*x*y, x^2*y - 2*y^2 + x);
      leadTerm I
      gens gb I
    Text
      The above example also shows that the leading terms of any set of
      generators of I do not necessarily generate in(I).

      A Gröbner basis for an ideal depends on the monomial ordering used in the
      ring .
    Example
      R = ZZ/1277[x,y, MonomialOrder => Lex];
      I = ideal(x^3 - 2*x*y, x^2*y - 2*y^2 + x);
      gens gb I
  SeeAlso
    "monomial orderings"
    leadTerm
    "Gröbner bases"
    "normal forms"
///

doc ///
  Key
    "computing Groebner bases"
  Description
    Text
      Use this page when you want to run a Gröbner-basis computation directly,
      extract the computed basis, or stop after a partial computation. Gröbner
      bases are computed with the @TT "gb"@ command; see @TO "gb"@. It returns
      an object of class @TT "GroebnerBasis"@.
    Example
      R = ZZ/1277[x,y];
      I = ideal(x^3 - 2*x*y, x^2*y - 2*y^2 + x);
      g = gb I
    Text
      To get the polynomials in the Gröbner basis, use @TT "gens"@
    Example
      gens g
    Text
      How do we control the computation of Gröbner bases? If we are working
      with homogeneous ideals, we may stop the computation of a Gröbner basis
      after S-polynomials up to a certain degree have been handled, with the
      option @TO "DegreeLimit"@. (This is meaningful only in homogeneous
      cases.)
    Example
      R = ZZ/1277[x,y,z,w];
      I = ideal(x*y-z^2,y^2-w^2);
      g2 = gb(I,DegreeLimit => 2)
      gens g2
    Text
      The result of the computation is stored internally, so when @TT "gb"@ is
      called with a higher degree limit, only the additionally required
      computation is done.
    Example
      g3 = gb(I,DegreeLimit => 3);
      gens g3
    Text
      The second computation advances the state of the Gröbner basis object
      started by the first, and the two results are exactly the same Gröbner
      basis object.
    Example
      g2
      g2 === g3
    Text
      The option @TO "PairLimit"@ can be used to stop after a certain number of
      S-polynomials have been reduced. After being reduced, the S-polynomial is
      added to the basis, or a syzygy has been found.
    Example
      I = ideal(x*y-z^2,y^2-w^2)
      gb(I,PairLimit => 2)
      gb(I,PairLimit => 3)
    Text
      The option @TO "BasisElementLimit"@ can be used to stop after a certain
      number of basis elements have been found.
    Example
      I = ideal(x*y-z^2,y^2-w^2)
      gb(I,BasisElementLimit => 2)
      gb(I,BasisElementLimit => 3)
    Text
      The option @TO "CodimensionLimit"@ can be used to stop after the apparent
      codimension, as gauged by the leading terms of the basis elements found
      so far, reaches a certain number.

      The option @TO "SubringLimit"@ can be used to stop after a certain number
      of basis elements in a subring have been found. The subring is determined
      by the monomial ordering in use. For @TT "Eliminate n"@ the subring
      consists of those polynomials not involving any of the first @TT "n"@
      variables. For @TT "Lex"@ the subring consists of those polynomials not
      involving the first variable. For @TT "ProductOrder {m,n,p}"@ the subring
      consists of those polynomials not involving the first @TT "m"@ variables.

      Here is an example where we are satisfied to find one relation from which
      the variable @TT "t"@ has been eliminated.
    Example
      R = ZZ/1277[t,F,G,MonomialOrder => Eliminate 1];
      I = ideal(F - (t^3 + t^2 + 1), G - (t^4 - t))
      transpose gens gb (I, SubringLimit => 1)
    Text
      Sometimes a Gröbner basis computation can seem to last forever. An
      ongoing visual display of its progress can be obtained with
      @TO "gbTrace"@.
    Example
      gbTrace = 3
      I = ideal(x*y-z^2,y^2-w^2)
      gb I
    Text
      Here is what the tracing symbols indicate.
    Pre
      {2}   ready to reduce S-polynomials of degree 2
      (0)   there are 0 more S-polynomials (the basis is empty)
       g    the generator yx-z2 has been added to the basis
       g    the generator y2-w2 has been added to the basis
      {3}   ready to reduce S-polynomials of degree 3
      (1)   there is 1 more S-polynomial
       m    the reduced S-polynomial yz2-xw2 has been added to the basis
      {4}   ready to reduce S-polynomials of degree 4
      (2)   there are 2 more S-polynomials
       m    the reduced S-polynomial z4-x2w2 has been added to the basis
       o    an S-polynomial reduced to zero and has been discarded
      {5}   ready to reduce S-polynomials of degree 5
      (1)   there is 1 more S-polynomial
       o    an S-polynomial reduced to zero and has been discarded
    Text
      Let's turn off the tracing.
    Example
      gbTrace = 0
    Text
      Each of the operations dealing with Gröbner bases may be interrupted or
      stopped (by typing CTRL-C). The computation is continued by re-issuing
      the same command. Alternatively, the command can be issued with the
      option @TT "StopBeforeComputation => true"@. It will stop immediately,
      and return a Gröbner basis object that can be inspected with
      @TO2(generators, "gens")@ or @TO "syz"@. The computation can be continued
      later.
    Example
      R = ZZ/1277[x..z];
      I = ideal(x*y+y*z, y^2, x^2);
      g = gb(I, StopBeforeComputation => true)
      gens g
    Text
      The function @TO "forceGB"@ can be used to create a Gröbner basis object
      with a specified Gröbner basis. No computation is performed to check
      whether the specified basis is a Gröbner basis.

      If the Poincare polynomial (or Hilbert function) for a homogeneous
      submodule @TT "M"@ is known, you can speed up the computation of a
      Gröbner basis by informing the system. This is done by storing the
      Poincare polynomial in @TT "M.cache.poincare"@.

      As an example, we compute the Gröbner basis of a random ideal, which is
      almost certainly a complete intersection, in which case we know the
      Hilbert function already.
    Example
      R = ZZ/1277[a..e];
      T = (degreesRing R)_0
      f = random(R^1,R^{-3,-3,-5,-6});
      time betti gb f
    Text
      The matrix was randomly chosen, and we'd like to use the same one again,
      but this time with a hint about the Hilbert function, so first we must
      erase the memory of the Gröbner basis computed above.
    Example
      remove(f.cache,{false,0})
    Text
      Now we provide the hint and compute the Gröbner basis anew.
    Example
      poincare cokernel f = (1-T^3)*(1-T^3)*(1-T^5)*(1-T^6) -- cache poincare
      time betti gb f
    Text
      The computation turns out to be substantially faster.
  Subnodes
    gb
    groebnerBasis
    forceGB
    -- TODO: also add references to packages that compute GB
///


-- document {
--      Key => "What is a Groebner basis?",
--      "A Gröbner basis is a specific generating set
--      of an ideal or submodule over a polynomial ring, not usually minimal,
--      which has extremely nice properties, from which
--      it is reasonably easy to extract information about the ideal or submodule.",
--      "We first define and describe Gröbner bases in the important special case
--      of an ideal in a polynomial ring.  We then
--      describe Gröbner bases of submodules, and over more general rings.",
--      PARA{},
--      TEX "Let $R = k[x_1, ..., x_n]$ be a polynomial ring, over a field k,
--      and let I \\subset R be an ideal.  A term order on R is, by definition, a total
--      order, >,  on the monomials of R, which satisfies two conditions: (1)
--      m > 1, for every monomial m \\neq 1, and (2) the order is multiplicative:
--      m > n implies that mp > np, for all monomials m,n,p.",
--      PARA{},
--      "In Macaulay2, each ring has a multiplicative order associated with it.
--      The default is the graded reverse lexicographic order:",
--      EXAMPLE {
-- 	  "R = QQ[a..d,MonomialOrder=>GRevLex]",
--      	  "F = a^3 + d^2 + a*d + b*c + 1",
-- 	  "R = QQ[a..d,MonomialOrder=>RevLex]",
-- 	  "substitute(F,R)",
-- 	  "R = QQ[a..d,MonomialOrder=>Lex]",
-- 	  "substitute(F,R)",
-- 	  "R = QQ[a..d,Weights=>{1,1,0,0}]",
-- 	  "substitute(F,R)",
-- 	  "R = QQ[a..d,Weights=>{-1,0,0,0}]",
-- 	  "substitute(F,R)",
-- 	  "R = QQ[a..d,Weights=>{-1,-1,-1,-1}]",
-- 	  "substitute(F,R)",
-- 	  "R = QQ[a..d,MonomialOrder=>ProductOrder{1,3}]",
-- 	  "substitute(F,R)"},
--      "Given a term order, the lead monomial is the term whose monomial is greatest
--      in this order.",
--      EXAMPLE "leadTerm F"
--      }

doc ///
  Key
    "fine control of a Groebner basis computation"
  Description
    Text
      Use this page when a Gröbner basis computation is too slow, too large, or
      needs to be inspected before it finishes. The main controls are the
      options @TO "DegreeLimit"@, @TO "PairLimit"@, @TO "BasisElementLimit"@,
      @TO "CodimensionLimit"@, and @TO "SubringLimit"@; the inspection tools
      @TO gbSnapshot@ and @TO "gbTrace"@; and @TO forceGB@ for creating a
      Gröbner basis object from data already known to be a basis.

      @SUBSECTION "Partially computed Gröbner bases"@

      Suppose that you have computed part of a Gröbner basis. For example, you
      may have interrupted the computation using @KBD "Ctrl-C"@ (typing
      @KBD "C"@ while holding the @KBD "Ctrl"@ key down, in Emacs, you have to
      type @KBD "Ctrl-C"@ twice), or you may have given options requesting only
      partial computation.
    Example
      R = ZZ/32003[a..e];
      I = ideal(random(3,R),random(3,R),random(3,R))
      gens gb(I,PairLimit=>7);
    Text
      Get the Gröbner basis object:
    Example
      g = gb(I,StopBeforeComputation => true);
      leadTerm gens g
    Text
      We can make a Gröbner basis snapshot by using StopBeforeComputation, or
      @TO gbSnapshot@:
    Example
      gens gb(I,StopBeforeComputation => true)
      leadTerm gbSnapshot(I)
///

///

R = QQ[a..d,Weights=>{-1,0,0,0},Global=>false]
f = a+b^2+c^3-2*d^4+1+a*b*c*d
leadTerm f
leadCoefficient f
leadTerm(1,f)

M = image vars R
gbSnapshot(M)
gb(M,PairLimit=>2)
m1 = gbSnapshot(M)
gens gb M
///
