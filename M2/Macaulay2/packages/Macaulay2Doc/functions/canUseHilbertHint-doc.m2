doc ///
  Key
    canUseHilbertHint
    (canUseHilbertHint, Ring)
    (canUseHilbertHint, Ideal)
    (canUseHilbertHint, Module)
    (canUseHilbertHint, Matrix)
  Headline
    whether certain Groebner computations can make use of the Hilbert function
  Usage
    canUseHilbertHint m
  Inputs
    m:{Ring,Ideal,Module,Matrix}
  Outputs
    :Boolean
  Description
    Text
      Certain Groebner basis computations, when given homogeneous input, can run faster if
      the Hilbert function is know: the algorithm can use this information to bypass
      computing zero reductions in many cases.

      In order to be able to use a Hilbert function hint, currently the object must be
      in a singly graded polynomial ring (or quotient of such), over a field,
      with all generators of positive degree, and it must be either commutative, or skew-commutative.

      Given a ring, this function returns whether this holds for the ring.  For
      a module, matrix or ideal, this function additionally checks whether the input is
      homogeneous.
    Example
      R1 = ZZ/101[x,y,z, Degrees => {1,2,3}];
      assert canUseHilbertHint R1
      R2 = ZZ[x,y,z];
      assert not canUseHilbertHint R2
      R3 = (GF 8)[x,y,z]
      assert canUseHilbertHint R3
      use R1
      R4 = R1[u,v, Join => false]/(x*u+y*v)
      R5 = first flattenRing R4
      canUseHilbertHint R4
      canUseHilbertHint R5
    Text
      Here is one way to use Hilbert hints. If one knows the Hilbert function, one can use it.
      If one computes a Hilbert function (via poincare), one can use that in a ring
      which is identical, except the monomial order is more computationally intensive (e.g. Lex).
    Example
      R = ZZ/101[x,y,z,w]
      I = ideal random(R^1, R^{-5,-5,-5})
      hf = poincare I
      codim I == 3 -- a complete intersection
      Rlex = newRing(R, MonomialOrder => Lex)
      Ilex = sub(I, Rlex)
      elapsedTime g1 = gens gb Ilex;
      Ilex = ideal(Ilex_*) -- clear out the previous Groebner basis
      elapsedTime g2 = gens gb(Ilex, Hilbert => hf);
      g1 == g2
  Caveat
    One cannot currently use Hilbert hints for multigraded input
  SeeAlso
    poincare
    newRing
///
