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
numeric CCC := identity
numeric RRR := identity

pi=3.1415926535897932384626433832795028841971693993

Constant = new Type of HashTable
net Constant := c -> net c.Symbol
expression Constant := c -> expression c.Symbol
toString Constant := c -> toString c.Symbol
toExternalString Constant := c -> toString c.Symbol
toRRR(ZZ,Constant) := (prec,c) -> c.toRRR prec
toCCC(ZZ,Constant) := (prec,c) -> c.toCCC prec
Constant + RRR := (c,x) -> toRRR(precision x,c) + x
RRR + Constant := (x,c) -> x + toRRR(precision x,c)
Constant - RRR := (c,x) -> toRRR(precision x,c) - x
RRR - Constant := (x,c) -> x - toRRR(precision x,c)
Constant * RRR := (c,x) -> toRRR(precision x,c) * x
RRR * Constant := (x,c) -> x * toRRR(precision x,c)
Constant / RRR := (c,x) -> toRRR(precision x,c) / x
RRR / Constant := (x,c) -> x / toRRR(precision x,c)

Pi = new Constant from {
     symbol Symbol => symbol Pi,
     symbol toRRR => mpfrConstantPi
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
