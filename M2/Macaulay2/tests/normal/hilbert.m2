-- https://github.com/Macaulay2/M2/issues/732
R=QQ[a..k]
I=trim ideal(a*b+c*d,a*e+b*f+c*g+d*h);
M=cokernel gens I
use degreesRing R
-- cache poincare
poincare M = 1-2*T^2+T^4;
-- FIXME
--assert(dim I == 9)

-- https://github.com/Macaulay2/M2/issues/974
R = QQ[x,y]
H = hilbertSeries ideal{x}
-- FIXME
--value H

--
R = QQ[a..d];
I = ideal random(R^1, R^{3:-3});
time hf = poincare I
S = QQ[a..d,MonomialOrder=>Eliminate 2]
J = substitute(I,S)
-- cache poincare
poincare J = hf
gbTrace=3
time gens gb J;
selectInSubring(1,gens gb J)

-- Now check it for matrices
R = QQ[a..d];
I = ideal random(R^1, R^{3:-3});
time hf = poincare I
S = QQ[a..d,MonomialOrder=>Eliminate 2]
J = substitute(I,S)
-- cache poincare
poincare gens J = hf
gbTrace=3
time gens gb gens J;
selectInSubring(1,gens gb J)

-- Now check it for modules
R = QQ[a..d];
I = image random(R^1, R^{3:-3});
time hf = poincare I
S = QQ[a..d,MonomialOrder=>Eliminate 2]
J = substitute(I,S)
-- cache poincare
poincare J = hf
gbTrace=3
time gens gb J
--status: this is a strange one
--status: it's a gb computation that seems to run out of memory far too soon
--status: Mike?
selectInSubring(1,gens gb J)

--
scan(3, n -> (
     x := local x;
     R := ZZ/101[x_0 .. x_n];
     scan(-2 .. 2, d -> (
	  M := R^{-d};
	  h := hilbertPolynomial M;
	  scan(d .. d + 4, e -> assert(numgens source basis(e,M) == h e))))))

--
scan(3, n -> (
     x := local x;
     R := ZZ/101[x_0 .. x_n];
     scan(-2 .. 2, d -> (
	  M := R^{-d};
	  h := hilbertPolynomial (M, Projective => false);
	  i := (ring h)_0;
	  scan(d .. d + 4, e -> (
		    r := numgens source basis(e,M);
		    s := substitute(h, { i => e/1 });
		    assert( r == s)))))))

--
importFrom_Core {"truncateSeries"}
R = QQ[x,y, DegreeRank => 2]
assert(truncateSeries(5, {1,1}, Divide{1_R,Product{Power{1-x,1}}}) == x^4+x^3+x^2+x+1)
assert(truncateSeries(5, {1,1}, Divide{1_R,Product{Power{1+x,1}}}) == x^4-x^3+x^2-x+1)
assert(truncateSeries(5, {1,1}, Divide{1_R,Product{Power{1+x,1},Power{1-x,1}}}) == x^4+x^2+1)
assert(truncateSeries(3, {1,1}, Divide{1_R,Product{Power{1+x,1},Power{1-y,1}}}) == x^2-x*y+y^2-x+y+1)
