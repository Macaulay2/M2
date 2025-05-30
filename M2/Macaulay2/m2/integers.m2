--		Copyright 1993-2002 by Daniel R. Grayson

needs "rings.m2"

-----------------------------------------------------------------------------
-- Number
-----------------------------------------------------------------------------

isHomogeneous Number := x -> true
ring Number := class
degree Number := i -> {}
conjugate Number := identity
toExternalString Number := simpleToString
floor Number := x -> floor0(x)
floor Constant := floor0 @@ numeric
ceiling Number := x -> - floor(-x)

-----------------------------------------------------------------------------
-- ZZ
-----------------------------------------------------------------------------

ZZ.synonym = "integer"
ZZ.texMath = ///{\mathbb Z}///

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
texMath ZZ := toString

gcd = method(Binary => true)
installMethod(gcd, () -> 0)
gcd(ZZ,ZZ) := ZZ => gcd0
gcd(ZZ,QQ) := QQ => (x,y) -> gcd(x * denominator y, numerator y) / denominator y
gcd(QQ,ZZ) := QQ => (y,x) -> gcd(x * denominator y, numerator y) / denominator y
gcd(QQ,QQ) := QQ => (x,y) -> (
     d := denominator x * (denominator y // gcd(denominator x, denominator y));
     gcd(numerator (x * d), numerator (y * d)) / d)
gcd ZZ := gcd QQ := identity

abs = method()
abs ZZ := abs RR := abs RRi := abs CC := abs QQ := abs0
abs Constant := abs @@ numeric

sign = method()
sign Number := sign0
sign Constant := sign @@ numeric

lcm = method(Binary => true)
installMethod(lcm, () -> 1)
lcm(ZZ,ZZ) := (f,g) -> (
    d := gcd(f, g);
    if d == 0 then 0
    else abs f * (abs g // d))
lcm(ZZ,QQ) :=
lcm(QQ,ZZ) :=
lcm(QQ,QQ) := (f,g) -> (
    d := gcd(f, g);
    if d == 0 then 0_QQ
    else abs f * (abs g / d))
lcm ZZ := lcm QQ := identity

odd  = x -> 1 === x%2
even = x -> 0 === x%2
zero = x -> x == 0					    -- we use == so this can apply to all types of things

isPrime       = method(TypicalValue => Boolean, Options => true)
isPseudoprime = method(TypicalValue => Boolean, Options => true)
isPrime       ZZ := Boolean => {} >> o -> n -> rawZZisPrime n -- calls flint
isPseudoprime ZZ := Boolean => {} >> o -> n -> rawZZisProbablePrime n -- calls flint

random ZZ := ZZ => opts -> n -> if n > 0 then rawRandomZZ n else error "random: expected a positive integer"
random(ZZ,ZZ) := ZZ => opts -> (min,max) -> (
     if min > max then error "random: empty range";
     min + rawRandomZZ(max-min+1)
     )
isUnit ZZ := x -> x == 1 or x == -1

ZZ & ZZ := ZZ => lookup(symbol &, ZZ, ZZ)

ZZ ^^ ZZ := bitxorfun
Boolean xor Boolean := (x, y) -> x and not y or not x and y

Function and Function := (f, g) -> s -> f s and g s
Function or  Function := (f, g) -> s -> f s or  g s
Function xor Function := (f, g) -> s -> f s xor g s
not Function := f -> s -> not f s

ZZ~ := bitnotfun

changeBase = method()
changeBase(ZZ,     ZZ)     := String =>
changeBase(String, ZZ)     := ZZ     => changeBase0
changeBase(String, ZZ, ZZ) := String => (s, oldbase, newbase) -> (
    changeBase(changeBase(s, oldbase), newbase))

-----------------------------------------------------------------------------
-- AtomicInt
-----------------------------------------------------------------------------

AtomicInt.synonym = "atomic integer"

scan({symbol +=, symbol -=, symbol &=, symbol |=, symbol ^^=},
    op -> typicalValues#(op, AtomicInt) = ZZ)

store = method()
store(AtomicInt, ZZ) := atomicStore

exchange = method()
exchange(AtomicInt, ZZ) := atomicExchange

compareExchange = method()
compareExchange(AtomicInt, ZZ, ZZ) := atomicCompareExchange

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
