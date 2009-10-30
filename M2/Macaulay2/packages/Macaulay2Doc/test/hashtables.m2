end

print generateAssertions ///
plus' = (x,y) -> ( z := x+y ; if z == 0 then continue ; z )
Poly = new Type of HashTable
one = new Poly from {1 => 1}
T = new Poly from {2 => 1}
- Poly := (f) -> applyValues(f,minus);
Poly + Poly := (f,g) -> merge(f,g,plus');
Poly - Poly := (f,g) -> f + -g;
Poly * Poly := (f,g) -> combine(f,g,times,times,plus');
Poly ^ ZZ := lookup(symbol ^,CC,ZZ)
f = one+T+T^2+T^3+T^4+T^5;
(T-one)*f
f - (T^3 + T^5)
///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test hashtables.out"
-- End:
