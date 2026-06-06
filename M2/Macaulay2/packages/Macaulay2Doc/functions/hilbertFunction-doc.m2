--- author(s): L. Gold, Dan Grayson

undocumented {}

doc ///
  Key
    hilbertFunction
    (hilbertFunction,List,Ring)
    (hilbertFunction,ZZ,Ring)
    (hilbertFunction,List,Module)
    (hilbertFunction,ZZ,Module)
    (hilbertFunction,List,Ideal)
    (hilbertFunction,ZZ,Ideal)
    (hilbertFunction,Ring)
    (hilbertFunction,Module)
    (hilbertFunction,Ideal)
  Headline
    the Hilbert function
  Usage
    hilbertFunction(d,X)
  Inputs
    d:{ZZ,List} -- of integers
      specifying a degree (or multidegree)
    M:{Ring,Ideal,Module}
  Outputs
    :ZZ
      the dimension of the degree @SAMP "d"@ part of @SAMP "M"@.  For an
      ideal, the corresponding quotient ring is used.
  Description
    Text
      In the following example, compare the rank of the source of the basis map
      to the number provided by @SAMP "hilbertFunction"@.
    Example
      R = QQ[x,y,z, Degrees=>{3:{1,1}}];
      hilbertFunction({3,3}, R)
      basis({3,3},R)
    Text
      The standard meaning of subscripts on functions permits a simpler syntax
      to be used.
    Example
      hilbertFunction_{3,3} R
    Text
      Here is a singly graded example.
    Example
      R = QQ[x,y,z];
      hilbertFunction({3}, R)
      hilbertFunction(3, R)
    Text
      Here is an example with a module.
    Example
      R = QQ[a..d, Degrees=>{4:{1,1}}];
      M = coker matrix {{a,c,d},{c,b,d}}
      hilbertFunction({2,2}, M)
      B = basis({2,2},M)
      numgens source B
    Text
      Here is an example with an ideal.
    Example
      R = QQ[a..f, Degrees=>{6:{1,1}}];
      I = ideal (a*b, c*d, e*f);
      hilbertFunction({2,2}, I)
      S = R/I;
      basis({2,2},S)
    Text
      If @SAMP "d"@ is not given, then a function is returned that will accept
      different values of @SAMP "d"@.
    Example
      R = QQ[a..d];
      I = monomialCurveIdeal(R, {1,2,3});
      h = hilbertFunction I
      h 1
      h 2
  Caveat
    It can be much faster to compute a basis for the desired degree,
    because hilbertFunction works by expanding the Hilbert series to a
    sufficiently high order, thus, in effect, computing many values of
    the Hilbert function simultaneously.  If several values of the
    Hilbert function are desired, it is best to compute the ones of
    higher degree first, so the expansion will be done to sufficiently
    high order at the first attempt, and thus be done just once.
  SeeAlso
    degreesRing
    reduceHilbert
    poincare
    "Complexes :: poincareN"
    hilbertSeries
    hilbertPolynomial
    numgens
    (symbol _, Function, Thing)
///
