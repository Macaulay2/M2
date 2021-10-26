debug needsPackage "NumericalAlgebraicGeometry"
R = CC[x]
f = (x-3) * (x-2) * x * (x+1)
g = (x-5) * (x-1) * (x+3) * (x+4)
S = CC[x,t]
H = matrix {{t*sub(g,S) + (1-t)*sub(f,S)}}
(t0, t1) = (0, 1)



trackSegment = (H, t0, t1, startSols) -> (
    R := ring H;
    t := last gens R;
    Rx := coefficientRing R[drop(gens R, -1)];
    m0 := map(Rx, R, gens Rx | {t0});
    m1 := map(Rx, R, gens Rx | {t1});
    f := m0 H;
    g := m1 H;
    sols := track(polySystem f, polySystem g, startSols);
    apply(sols, s -> (
	    s.LastT = (1-s.LastT)*t0 + s.LastT *t1;
	    s
	    ))
    )    
end

restart
load "quadratic_tp_experiment.m2"

sols = trackSegment(H, 0, 1, {point{{3}},point{{2}},point{{0}},point{{-1}}})
x0 = (1/2) * (matrix sols#2 + matrix sols#3)
t0 = (sols#2).LastT

J = evaluate(transpose jacobian H, x0 | matrix{{t0}})
n = numRows H
v = submatrix'(((SVD J)#2)_{n}, {n}, )
epsilon = 0.1
x1 = x0 + epsilon * ii * v

toR = map(R, S, gens R | {t0+epsilon^2})

x1 = newton(polySystem toR H, x1)
x1 = newton(polySystem toR H, x1)
x1 = newton(polySystem toR H, x1)
x1 = newton(polySystem toR H, x1)
x1 = newton(polySystem toR H, x1)

trackSegment(H, t0 +epsilon^2, 1, {point(x1)})
solutions = oo
peek solutions
peek solutions#2
peek solutions

-- reasonable eps, reasonable coef, complex => real, better examples


solveSystem {sub(sub(H, {t=> 0.03}),R)}

options(trackHomotopy)
trackHomotopy
