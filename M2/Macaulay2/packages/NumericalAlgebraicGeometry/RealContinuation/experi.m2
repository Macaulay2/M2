restart
debug needsPackage "NumericalAlgebraicGeometry"
load "quadratic_tp_experiment.m2"
needs "example_2d.m2"

R = CC[x]
f = (x-3) * (x-2) * x * (x+1)
g = (x-5) * (x-1) * (x+3) * (x+4)
S = CC[x,t]
h = matrix {{t*sub(g,S) + (1-t)*sub(f,S)}}
(t0, t1) = (0, 1)
sols = trackSegment(H, 0, 1, {point{{3}},point{{2}},point{{0}},point{{-1}}})

peek sols#2
x0 = ((1/2) * (matrix sols#2 + matrix sols#3))_(0,0)
t0 = sub((sols#2).LastT,CC)
lli = sub(.05,CC)

approxi ={{x0,t0}}



deflationMethod = (H, approx, ll) -> (
    ldimm := # gens R;
    R'' := CC[x,t,l_1..l_ldimm];
    H'' := sub(H,R'');
    l' := matrix{drop (gens R'',{0,1})};
    JJ := sub(submatrix'(jacobian H,{1},),R'');
    JJ' := JJ*l';
    vv := random(CC^1,CC^ldimm);
    vv' := vv*transpose l'+1;
    N' := polySystem{H'',JJ',vv'};
    pp := point{append(flatten approx, ll)};
    Approx :=  newton(N',pp)
    )
deflationMethod(h, approxi, lli)
