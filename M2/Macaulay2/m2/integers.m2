--		Copyright 1993-2002 by Daniel R. Grayson

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

promote(ZZ,ZZ) := lift(ZZ,ZZ) := (i,ZZ) -> i
liftable'(ZZ,ZZ) := x -> true

ZZ.random = opts -> (
     h := opts.Height;
     random (2 * h + 1) - h)

oldgcd := gcd
erase symbol gcd

gcd = method(Binary => true)
gcd List := x -> gcd toSequence x
gcd(ZZ,ZZ) := ZZ => (x,y) -> oldgcd(x,y)
gcd(ZZ,QQ) := QQ => (x,y) -> gcd(x * denominator y, numerator y) / denominator y
gcd(QQ,ZZ) := QQ => (y,x) -> gcd(x * denominator y, numerator y) / denominator y
gcd(QQ,QQ) := QQ => (x,y) -> (
     d := denominator x * (denominator y // gcd(denominator x, denominator y));
     gcd(numerator (x * d), numerator (y * d)) / d)

smallprimes := {2,3,5,7,11,13,17,23,29,31,37,41,43,47}

isPrime1 := n -> member(n,smallprimes) or all(smallprimes,p -> n%p =!= 0)

isPrime2 := n -> (			  -- assume n > 2
     a := 2;				  -- base for pseudo-primality
     n1 := n-1;
     n2 := 1;
     while even n1 do (n1 = n1//2; n2 = 2*n2;);
     kk := (-1) % n;
     k := powermod(a,n1,n);
     while k =!= 0 and k =!= 1 and n2 > 1 do (
	  kk = k;
	  k = k^2 % n;
	  n2 = n2 // 2;
	  );
     k === 1 and kk === (-1) % n)

biggest := 2^31-1

isPrime ZZ := Boolean => n -> (
     n = abs n;
     n > 1 and
     if n <= biggest 
     then (
	  v := factor n;
	  # v === 1 and v#0#1 === 1
	  )
     else isPrime1 n and (n == 2 or isPrime2 n)
     )

random ZZ := ZZ => opts -> x -> (
     if x <= 0 then error "expected a positive number";
     rawRandomInteger x)

random(ZZ,ZZ) := ZZ => opts -> (min,max) -> min + rawRandomInteger(max-min+1)


ceiling = x -> - floor(-x)

isUnit ZZ := x -> x == 1 or x == -1

ZZ & ZZ := ZZ => lookup(symbol &, ZZ, ZZ)

isConstant ZZ := i -> true

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
