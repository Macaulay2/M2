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

isPseudoprime ZZ := Boolean => Pari$ispseudoprime

isPrime ZZ := Boolean => (
     if instance(Pari$isprime,Function) then Pari$isprime
     else 
     n -> (
	  n = abs n;
	  n > 1 and
	  if n <= biggest 
	  then (
	       v := factor n;
	       # v === 1 and v#0#1 === 1
	       )
	  else isPrime1 n and (n == 2 or isPrime2 n)
	  ))

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
