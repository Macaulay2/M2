--- status: Draft
--- author(s): Amelia Taylor
--- notes: Be sure to note that trim I and trim R^1/I do 
---        the same thing as the minimal relations for 
---        R^1/I are the minimal generators for I.

doc ///
Node
  Key
    MinimalGenerators
  Headline
    whether to compute minimal generators and return a trimmed set of generators
  Description
    Text
      The following returns two minimal generators (Serre's Theorem: a codim 2 Gorenstein ideal is a complete intersection.)
    Example
      S = ZZ/101[a,b]
      i = ideal(a^4,b^4)
      quotient(i, a^3+b^3)
    Text
      Without trimming we would get 4 generators instead.
    Example
      quotient(i, a^3+b^3, MinimalGenerators => false)
    Text
      Sometimes the extra time to find the minimal generators is too large.
      This allows one to bypass this part of the computation.
    Example
      R = ZZ/101[x_0..x_4]
      I = truncate(8, monomialCurveIdeal(R,{1,4,5,9}));
      time gens gb I;
      time J1 = saturate(I);
      time J = saturate(I, MinimalGenerators => false);
      numgens J
      numgens J1
  SeeAlso
    "Saturation :: quotient(Ideal,Ideal)"
    "Saturation :: saturate(Ideal,Ideal)"
    monomialCurveIdeal
    "Truncations::truncate"

-- this is the old version
Node
  Key
    trim
   (trim, Ideal)
   (trim, Ring)
   (trim, Module)
   (trim, QuotientRing)
  Headline
    minimize generators and relations
  Description
    Text
      There are two ways to present an $R$-module $M$. One way is to take a free module $F$
      (whose generators are called the @TO generators@) and form the quotient $M = F/H$ by a submodule
      $H\subset F$ (whose generators are called the @TO relations@).

      Another way is take a free module $F$,
      a submodule $G\subset F$ (whose generators are called the @TO generators@),
      a submodule $H\subset F$ (whose generators are called the @TO relations@), and
      form the @TO2 {"subquotient modules", "subquotient module"}@ $M = (G+H)/H$,
      obtained also as the image of $G$ in $F/H$.

      The purpose of @TT "trim"@ is to minimize presentations of the latter type.
      This applies also to rings and ideals.
    Example
      R = ZZ/101[x,y,z,u,w]
      I = ideal(x^2-x^2-y^2, z^2+x*y, w^2-u^2, x^2-y^2)
      trim I
      trim (R^1/I)
    Example
      R = ZZ/32003[a..d]
      M = coker matrix {{a,1,b},{c,3,b+d}}
      trim M
      prune M
  SeeAlso
    prune
    mingens
    "subquotient modules"
    generators
    relations
    subquotient
///
