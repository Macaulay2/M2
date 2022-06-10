GF 25
assert( 24 == # unique apply(24, i -> a^i) )

debug Core
GF 97^2 [x]
assert ( toString x == "x" )
rawPairs(raw coefficientRing class x,raw x)
GF 101^2 [x]
assert ( toString x == "x" )
rawPairs(raw coefficientRing class x,raw x)

-- This works in 1.1
debug Core
R = ZZ/101[a]/(a^2+47*a-26)
toField R
K = GF R
S = K[x]
assert(toString promote(a,S) == "a")
raw promote(a,S)  -- now it works

-- git issue 978 FIXED
  rands = for i from 1 to 10 list toString (
    k = GF(81);
    random(k^2, k^2)
    )
  assert(# unique rands > 1)

end--
------------------------------------
gives (gave) this:  fixed 11/7/08

    i1 : debug Core

    i2 : GF 97^2 [x]

    o2 = GF 9409[x]

    o2 : PolynomialRing

    i3 : assert ( toString x == "x" )

    i4 : rawPairs(raw coefficientRing class x,raw x)

    o4 = (1 : (1), 1 : (a))

    o4 : Sequence

    i5 : GF 101^2 [x]

    o5 = GF 10201[x]

    o5 : PolynomialRing

    i6 : assert ( toString x == "x" )
    stdio:6:1:(2):[3]: error: assertion failed

    i7 : rawPairs(raw coefficientRing class x,raw x)

    o7 = (1 : (1), 1 : (1))

    o7 : Sequence

The difference between the two fields is that the larger one is constructed without using rawGaloisField
