restart
debug needsPackage "NumericalAlgebraicGeometry"
load "quadratic_tp_experiment.m2"
needs "example_2d.m2"

setRandomSeed 1
R = CC[x,y]
S = CC[x,y,t]
(d1,d2) = (3,3)
f = {sub(randomPolynomial(d1, RR[gens R]), R),  sub(randomPolynomial(d2, RR[gens R]),R)}

f' = {sub(sub(randomPolynomial(d1, RR[gens R]), R),S),  sub(sub(randomPolynomial(d2, RR[gens R]),R),S)}
ps = polySystem f 
startSols = solveSystem polySystem ps
g = {sub(randomPolynomial(d1, RR[gens R]), R),  sub(randomPolynomial(d2, RR[gens R]),R)}
g' = {sub(sub(randomPolynomial(d1, RR[gens R]), R),S),  sub(sub(randomPolynomial(d2, RR[gens R]),R),S)}

fm = matrix {flatten f}
gm = matrix {flatten g}
S = CC[x,y,t]
fmm = sub(fm, S)
gmm = sub(gm, S)
h = matrix{{t*gmm + (1-t)*fmm}}
findTurningPoints track(f,g,startSols)
(t0, t1) = (0, 1)

sols = track(f,g, startSols)
netList sols
peek sols#0
coordinates sols#0
x0 = ((1/2) * (matrix sols#0 + matrix sols#3))_(0,0)
t0 = sub((sols#0).LastT,CC)
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
