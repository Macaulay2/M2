load "quadratic_tp_experiment.m2"

randomPolynomial = (d, R) -> sum(d+1, i -> random(i,R))


findTurningPoints = L -> (

    if #temp === 0 then null
    else {temp#0#1, temp#1#1}
    )

sortTurningPoints = L -> apply(sort apply(L, sol -> {sol.LastT, sol}), last)    

-- will replace with better method (deflation)
getXo = tp ->   x0 = (1/2) * (matrix first tp + matrix last tp)

getTangent = (tp, x0, H) -> (
   t0 := (first tp).LastT;
   J := evaluate(transpose jacobian transpose H, x0 | matrix{{t0}});
   n := numRows H;
   transpose submatrix'(((SVD J)#2)^{n}, ,{n} )
    ) 

-- tp is a pair of points approaching a turning point
jump = (tp, H, epsilon) -> (
    if tp === null then error("no turning points inputted");
    x0 := getXo(tp);
    v := getTangent(tp, x0, H);    
    if any((first tp).Coordinates | (last tp).Coordinates, c -> not areEqual(imaginaryPart c, 0)) then 
    l1 := {transpose x0 + epsilon * v, transpose x0 - epsilon * v }
    else         (
	print "two conjugate nonreal";
	x1 := transpose x0 + epsilon * ii * v;
	l1 = {x1, conjugate x1};
	);
    R := RR[drop(gens ring H, -1)];
    toR := map(R, ring H, gens R | {t0+epsilon^2});
    -- needs a better stopping criterion
    for i from 1 to 5 do l1 = apply(l1, l -> newton(polySystem toR H, l));
    l1
    )

solveRealSystem = method()
solveRealSystem (PolySystem, PolySystem, List) := (f, g, startSols) -> (
    if ring f =!= ring g then error "expected systems from same ring";
    R := ring f;
    t := local t;
    S := CC (monoid[gens R,t]);
    t = last gens S;
    H := transpose (t*sub(gens ideal g,S) + (1-t)*sub(gens ideal f,S));
    temp := trackSegment(H,0,1,startSols);
    sols := select(temp, s -> status s != Regular);
    finishedSols := select(temp, s -> status s == Regular);
    while sols != {} do (
	sols' := sortTurningPoints(sols);
	if #sols' === 1 or not areEqual((sols'#0).LastT, (sols'#1).LastT)  then error "expected two failed paths";
        epsilon := 0.01;
	tp := take(sols', 2);
        jumped := jump(tp, H, epsilon);
        t0 := (tp#0).LastT;
	tp' := trackSegment(H, t0 +epsilon^2, 1, {point(jumped#0), point(jumped#1)});
	sols = drop(sols,2);
	for s in tp' do 
	if status s == Regular then finishedSols = finishedSols | {s}
	else sols = sols | {s};       		
	);
    finishedSols
    )

end

restart
needs "example_2d.m2"

setRandomSeed 1
R = CC[x,y]
(d1,d2) = (3,3)
f = polySystem {sub(randomPolynomial(d1, RR[gens R]), R),  sub(randomPolynomial(d2, RR[gens R]),R)}
startSols = solveSystem polySystem f
g = polySystem {sub(randomPolynomial(d1, RR[gens R]), R),  sub(randomPolynomial(d2, RR[gens R]),R)}
solveRealSystem(f,g,startSols)
findTurningPoints track(f,g,startSols)

(sols#1).Coordinates
epsilon = 0.01
tp = findTurningPoints sols
predictor = getPredictor(tp, epsilon)

sols2 = trackSegment(H, t0 +epsilon^2, 1, {point(predictor#0), point(predictor#1)})

epsilon = 0.01
tp = findTurningPoints sols2
predictor = getPredictor(tp, epsilon)
trackSegment(H, t0 +epsilon^2, 1, {point(predictor#0), point(predictor#1)})



solveSystem polySystem g
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
