variations = c -> (
     n := 0;
     last := 0;
     scan(c, x -> if x =!= 0 then (
	       if last < 0 and x > 0 or last > 0 and x < 0 then n = n+1;
	       last = x;
	       ));
     n)

SturmSequence = f -> (
     assert( isPolynomialRing ring f );
     assert( numgens ring f === 1 );
     R := ring f;
     x := R_0;
     n := first degree f;
     c := new MutableList from toList (0 .. n);
     if n >= 0 then (
     	  c#0 = f;
	  if n >= 1 then (
     	       c#1 = diff(x,f);
	       scan(2 .. n, i -> c#i = - c#(i-2) % c#(i-1));
	       ));
     toList c)

valueAtInfinity := leadCoefficient
valueAtMinusInfinity := g -> (if odd first degree g then -1 else 1) * leadCoefficient g

numberOfRealRoots = f -> (
     c := SturmSequence f;
     variations (valueAtMinusInfinity \ c) - variations (valueAtInfinity \ c)
     )

TEST ///
R = QQ[t]
g = (t-1)*(t-1/10)*(t+100)
h = (1 + (t^2-t)^2)*(1 + (t^3-t)^2)
f = g*h
assert(3 === numberOfRealRoots f)
assert(4 === numberOfRealRoots (f * (t + 10000)))
assert(3 === numberOfRealRoots (f * (t^2 + 10000)))
assert(0 === numberOfRealRoots h)
///
