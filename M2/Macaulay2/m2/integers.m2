--		Copyright 1993-2002 by Daniel R. Grayson

isHomogeneous Number := x -> true
ring Number := class
degree Number := i -> {}
conjugate Number := identity
toExternalString Number := simpleToString

ZZ.RawRing = rawZZ()
protect isBasic
ZZ.isBasic = true
new ZZ from RawRingElement := (ZZ,n) -> rawToInteger n
raw ZZ := x -> rawFromNumber(rawZZ(), x)

ZZ#1 = 1
ZZ#0 = 0
ZZ.char = 0
ZZ.InverseMethod = x -> 1/x
ZZ.dim = 1
ZZ.Engine = true
ZZ.baseRings = {}
ZZ.degreeLength = 0
ZZ.frac = QQ
round ZZ := identity
lift(ZZ,ZZ) := opts -> (i,ZZ) -> i
promote(ZZ,ZZ) := (i,ZZ) -> i
ZZ.random = opts -> ZZ -> rawRandomZZ opts.Height
gcd = method(Binary => true)
gcd List := x -> gcd toSequence x
installMethod(gcd, () -> 0)
gcd(ZZ,ZZ) := ZZ => gcd0
gcd(ZZ,QQ) := QQ => (x,y) -> gcd(x * denominator y, numerator y) / denominator y
gcd(QQ,ZZ) := QQ => (y,x) -> gcd(x * denominator y, numerator y) / denominator y
gcd(QQ,QQ) := QQ => (x,y) -> (
     d := denominator x * (denominator y // gcd(denominator x, denominator y));
     gcd(numerator (x * d), numerator (y * d)) / d)

lcm = method(Binary => true)
lcm List := x -> lcm toSequence x
lcm(ZZ,ZZ) := (f,g) -> abs f * (abs g // gcd(f,g))
lcm(ZZ,QQ) := (f,g) -> abs f * (abs g / gcd(f,g))
lcm(QQ,ZZ) := (f,g) -> abs f * (abs g / gcd(f,g))
lcm(QQ,QQ) := (f,g) -> abs f * (abs g / gcd(f,g))

odd  = x -> 1 === x%2
even = x -> 0 === x%2
zero = x -> x == 0					    -- we use == so this can apply to all types of things

isPseudoprime ZZ := Boolean => n -> rawZZisProbablePrime n -- calls flint

isPrime ZZ := Boolean => n -> rawZZisPrime n -- calls flint

random ZZ := ZZ => opts -> n -> if n > 0 then rawRandomZZ n else error "random: expected a positive integer"
random(ZZ,ZZ) := ZZ => opts -> (min,max) -> (
     if min > max then error "random: empty range";
     min + rawRandomZZ(max-min+1)
     )
floor = method()
floor Number := x -> floor0(x)
ceiling = method()
ceiling Number := x -> - floor(-x)
isUnit ZZ := x -> x == 1 or x == -1

ZZ & ZZ := ZZ => lookup(symbol &, ZZ, ZZ)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
