--		Copyright 1993-1999 by Daniel R. Grayson

odd  = x -> 1 === x%2

even = x -> 0 === x%2

zero = x -> x == 0					    -- we use == so this can apply to all types of things

numeric = method(Dispatch => Thing)
numeric Thing := identity
numeric VisibleList := x -> apply(x,numeric)
numeric Number := x -> x + 0.
numeric CC := identity
numeric RR := identity

pi=3.1415926535897932384626433832795028841971693993

Constant = new Type of HashTable
net Constant := c -> net c.Symbol
expression Constant := c -> expression c.Symbol
toString Constant := c -> toString c.Symbol
toExternalString Constant := c -> toString c.Symbol
toRR(ZZ,Constant) := (prec,c) -> c.toRR prec
toCC(ZZ,Constant) := (prec,c) -> c.toCC prec
Constant + RR := (c,x) -> toRR(precision x,c) + x
RR + Constant := (x,c) -> x + toRR(precision x,c)
Constant - RR := (c,x) -> toRR(precision x,c) - x
RR - Constant := (x,c) -> x - toRR(precision x,c)
Constant * RR := (c,x) -> toRR(precision x,c) * x
RR * Constant := (x,c) -> x * toRR(precision x,c)
Constant / RR := (c,x) -> toRR(precision x,c) / x
RR / Constant := (x,c) -> x / toRR(precision x,c)

Pi = new Constant from {
     symbol Symbol => symbol Pi,
     symbol toRR => mpfrConstantPi
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
