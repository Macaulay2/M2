restart
load "quadratic_tp_experiment.m2"

randomPolynomial = (d, R) -> sum(d+1, i -> random(i,R))

R = CC[x,y]
(d1,d2) = (2,2)
f = transpose matrix {{sub(randomPolynomial(d1, RR[gens R]), R),  sub(randomPolynomial(d2, RR[gens R]),R)}}
startSols = solveSystem polySystem f
g = transpose matrix {{sub(randomPolynomial(d1, RR[gens R]), R),  sub(randomPolynomial(d2, RR[gens R]),R)}}


S = CC[gens R,t]

H = t*sub(g,S) + (1-t)*sub(f,S)
(t0, t1) = (0, 1)

sols = trackSegment(H, 0, 1, startSols)
peek sols
rsort {{1,2}, {2,1}}
findTurningPoints = L -> (
    temp := sort apply(select(L, sol -> status sol =!= Regular), sol -> {sol.LastT, sol});
    if #temp === 0 then null
    else if #temp === 1 or not areEqual(temp#0#0, temp#1#0)  then error "expected two failed paths"
    else {temp#0#1, temp#1#1}
    )

tp = findTurningPoints sols
peek tp
x0 = (1/2) * (matrix first tp + matrix last tp)
t0 = (first tp).LastT

help SVD
J = evaluate(transpose jacobian transpose H, x0 | matrix{{t0}})
n = numRows H
v = transpose submatrix'(((SVD J)#2)^{n}, ,{n} )
J*(v || matrix {{0}})
epsilon = 0.1
x1 = transpose x0 - epsilon  * v

toR = map(R, S, gens R | {t0+epsilon^2})


x1 = newton(polySystem toR H, x1)
x1 = newton(polySystem toR H, x1)
x1 = newton(polySystem toR H, x1)
x1 = newton(polySystem toR H, x1)
x1 = newton(polySystem toR H, x1)

trackSegment(H, t0 +epsilon^2, 1, {point(x1)})

solutions = oo
x1
H = matrix {{t*sub(g,S) + (1-t)*sub(f,S)}}
(t0, t1) = (0, 1)


sols = trackSegment(H, 0, 1, {point{{3}},point{{2}},point{{0}},point{{-1}}})
x0 = (1/2) * (matrix sols#2 + matrix sols#3)
t0 = (sols#2).LastT


J
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


H = matrix {{t*sub(g,S) + (1-t)*sub(f,S)}}
(t0, t1) = (0, 1)


sols = trackSegment(H, 0, 1, {point{{3}},point{{2}},point{{0}},point{{-1}}})
x0 = (1/2) * (matrix sols#2 + matrix sols#3)
t0 = (sols#2).LastT


J
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


H = matrix {{t*sub(g,S) + (1-t)*sub(f,S)}}
(t0, t1) = (0, 1)


sols = trackSegment(H, 0, 1, {point{{3}},point{{2}},point{{0}},point{{-1}}})
x0 = (1/2) * (matrix sols#2 + matrix sols#3)
t0 = (sols#2).LastT


J
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
