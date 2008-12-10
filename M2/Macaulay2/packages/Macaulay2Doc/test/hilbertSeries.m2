-- hilbertSeries assumes the ring is a polynomial ring and gets into trouble when it's not.
-- To fix: check whether Mike's code handles the numerator correctly, first.

R = QQ[x,y]/x
M = R^1
fix = M -> (
     f := presentation ring M;
     coker f ** coker lift(presentation M,ring f))
M' = fix M
hilbertSeries M'
hilbertSeries M
assert( oo === ooo )
