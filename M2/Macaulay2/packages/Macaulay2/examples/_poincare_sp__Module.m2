R = ZZ/101[w..z];
M = module monomialCurveIdeal(R,{1,3,4});
poincare M
numerator reduceHilbert hilbertSeries M
R=ZZ/101[x, Degrees => {{1,1}}];
M = module ideal x^2;
poincare M
numerator reduceHilbert hilbertSeries M
