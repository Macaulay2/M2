R = ZZ/101[w..z];
I = monomialCurveIdeal(R,{1,3,4});
poincare I
numerator reduceHilbert hilbertSeries I
R=ZZ/101[x, Degrees => {{1,1}}];
I = ideal x^2;
poincare I
numerator reduceHilbert hilbertSeries I
