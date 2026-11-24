-- -*- coding: utf-8 -*-
--- status: Draft
--- author(s): M2Fest 2005 -- Irena
--- notes: 

undocumented{(frac, InexactField)}
doc ///
  Key
    frac
    (frac,Ring)
    (frac,FractionField)
    (frac,EngineRing)
  Headline
    construct a fraction field
  Usage
    frac R
  Inputs
    R:Ring
      an integral domain
  Outputs
    :FractionField
      the field of fractions of @TT "R"@
  Description
    Example
      F = frac ZZ
      F = frac (ZZ[a,b])
    Text
      After invoking the @TT "frac"@ command, the elements of the ring are
      treated as elements of the fraction field:
    Example
      R = ZZ/101[x,y];
      gens gb ideal(x^2*y - y^3)
      K = frac R;
      gens gb ideal(x^2*y - y^3)
    Text
      Another way to obtain @TT "frac R"@ is with @TT "x"@@TO "/"@@TT "y"@
      where @TT "x, y"@ are elements of @TT "R"@:
    Example
      a*b/b^4
    Text
      Fractions are reduced to the extent possible.
    Example
      f = (x-y)/(x^6-y^6)
      (x^3 - y^3) * f
    Text
      The parts of a fraction may be extracted.
    Example
      numerator f
      denominator f
    Text
      Alternatively, the functions @TO "lift"@ and @TO "liftable"@ can be used.
    Example
      liftable(1/f,R)
      liftable(f,R)
      lift(1/f,R)
    Text
      One can form resolutions and Gröbner bases of ideals in polynomial rings
      over fraction fields, as in the following example. Note that computations
      over fraction fields can be quite slow.
    Example
      S = K[u,v];
      I = ideal(y^2*u^3 + x*v^3, u^2*v, u^4);
      gens gb I
      Ires = res I
      Ires.dd_2
    Text
      One way to compute a blowup of an ideal @TT "I"@ in @TT "R"@, is to
      compute the kernel of a map of a new polynomial ring into a fraction
      field of @TT "R"@, as shown below.
    Example
      A = ZZ/101[a,b,c];
      f = map(K, A, {x^3/y^4, x^2/y^2, (x^2+y^2)/y^4});
      kernel f
  Caveat
    The input ring should be an integral domain.

    Currently, for @TT "S"@ as above, one cannot define @TT "frac S"@ or
    fractions @TT "u/v"@. One can get around that by defining
    @TT "B = ZZ/101[x,y,u,v]"@ and identify @TT "frac S"@ with @TT "frac B"@.

    Note that expressions such as @TT "frac QQ[x]"@ are parsed as
    @TT "(frac QQ)[x]"@. To obtain the fraction field of @TT "QQ[x]"@ use
    instead @TT "frac (QQ[x])"@.
  SeeAlso
    numerator
    denominator
    liftable
    lift
///

doc ///
Node
  Key
    fraction
   (fraction, RingElement, RingElement)
  Usage
    fraction(f, g)
  Inputs
  Outputs
    :RingElement
      the fraction @TT "f/g"@
  Description
    Text
      The output will be in the fraction field of the ring containing @TT "f"@ and @TT "g"@,
      without reducing it to lowest terms.
  SeeAlso
    frac
///
