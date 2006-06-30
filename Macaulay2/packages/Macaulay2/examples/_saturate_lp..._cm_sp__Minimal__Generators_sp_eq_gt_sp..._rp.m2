R = ZZ/101[x_0..x_4]
I = truncate(8, monomialCurveIdeal(R,{1,4,5,9}));
time gens gb I;
time J1 = saturate(I);
time J = saturate(I, MinimalGenerators=>false);
numgens J
numgens J1
