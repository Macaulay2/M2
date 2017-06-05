newPackage(
	"NumericalCertification",
    	Version => "0.5", 
    	Date => "July, 2016",
    	Authors => {
	     {Name => "Kisun Lee", Email => "klee669@gatech.edu"}
	     },
    	HomePage => "http://people.math.edu/~klee669",
    	Headline => "certifying solutions to square polynomial systems (Smale's alpha test)",
	PackageExports => {"NumericalAlgebraicGeometry"},
    	--DebuggingMode => true		 -- set to true only during development
    	DebuggingMode => false,
	AuxiliaryFiles => true
    	)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists

export {"absValue", "hermitianNorm", "oneNorm", "polyNorm", "polySysNorm", "complexToRational", "rationalToComplex", "computeConstants", "certifySolution", "certifyDistinctSoln", "frobeniusNormSq"}
exportMutable {}

-- in: a rational number a, precision parameter epsilon
-- out: a rational q(a) with q(a) >= sqrt(a) and |q(a) - sqrt(a) | <= epsilon
-- potentially of use in computing gamma parameter
sqrtUpper = method()
sqrtUpper (QQ, QQ) := (a, epsilon) -> (
    p := (1+a)/2;
    while abs(p^2 - a) >= epsilon * (p + min(a,1)) do (
	p = p - (p^2-a)/(p+a)
	);
    p
    )
sqrtUpper (ZZ, QQ) := (a, epsilon) -> (
    p := (1+a)/2;
    while abs(p^2 - a) >= epsilon * (p + min(a,1))  do (
	p = p - (p^2-a)/(p+a)
	);
    p
    )

absValue = method()
absValue ZZ := abs
absValue QQ := abs
absValue RR := abs
absValue CC := abs
absValue(RingElement) := r -> (
    R := ring(r);
    LT := leadTerm(sub(r,R));
    VV := (leadCoefficient(LT))^2 + (sub(r,R)-LT)^2;
    sqrt(sub(VV,RR))
    )



hermitianNorm = method()
hermitianNorm(Point) := x -> (
    N := sqrt(sum(apply(coordinates x, i -> absValue(i)^2)))
    )

oneNorm = method()
oneNorm(Point) := x -> (
    N := sqrt(1+(hermitianNorm(x)*hermitianNorm(x)))
    )

polyNorm = method()
polyNorm(RingElement) := r -> (
    L := listForm r;
    sum(L,a->(
	(e,c) := a;
	((absValue c))^2*(product(e,b->b!)*(((degree r)#0-(sum e))!)/((degree r)#0)!)
 	))
    )

polySysNorm = method()
polySysNorm(PolySystem) := f -> (
    listOfEq := equations f;
    listOfpolyNorms := apply( listOfEq, i -> (polyNorm(i))^2);
    N := sum listOfpolyNorms
    )

frobeniusNormSq = method()
frobeniusNormSq(Matrix) := r -> (
    a := flatten entries r;
    sum apply(a, s -> (absValue(s))^2)
    )


complexToRational = method()
complexToRational(ZZ, QuotientRing) := (x,FF) -> (
    sub(x,FF)
    )
complexToRational(QQ, QuotientRing) := (x,FF) -> (
    sub(x,FF)
    )
complexToRational(RR, QuotientRing) := (x,FF) -> (
    sub(promote(round(37, x),QQ),FF)
    )
complexToRational(CC, QuotientRing) := (x,FF) -> (
    im := promote(round(37, imaginaryPart x),QQ);
    re := promote(round(37, realPart x),QQ);
    Gaussian := re + im*((generators FF)#0)
    )
complexToRational(Point, QuotientRing) := (p, FF) -> (
    point matrix{complexToRational(coordinates p, FF)}
    )
complexToRational(List, QuotientRing) := (A, FF) -> (
    apply(A, a -> complexToRational(a, FF))
    )
complexToRational(Matrix, QuotientRing) := (M, FF) -> (
    entri := entries M;
    chan := apply(entri, s -> apply(s, ss -> complexToRational(sub(ss,CC), FF)));
    matrix chan
    )
complexToRational(PolySystem, QuotientRing) := (f, FF) -> (
    R := ring f;
--    coeffsinFF := complexToRational((coefficients f)#1, FF);
    varss := gens R;
    -- varss := apply(flatten entries vars R, x -> getSymbol(toString x));
--    x := symbol x;
--    y := symbol y;
    R' := FF(monoid [varss]);    
    ff := polySystem f;
    Mf := matrix applyTable(entries(f.PolyMap), a->
	sum(listForm a, ec->(
		(e,c) := ec;
		complexToRational(c,FF) * R'_e
		))
	);
    ff.PolyMap = Mf;    
    ff.Jacobian = transpose jacobian transpose Mf;
    ff
    )


rationalToComplex = method()
rationalToComplex CC := x -> x
rationalToComplex RR := x -> x
rationalToComplex QQ := x -> x

rationalToComplex PolySystem := f -> (
    R := ring f;
    varss := apply(flatten entries vars R, x -> getSymbol(toString x));
--    x := symbol x;
--    y := symbol y;
    phi := map(CC[varss], R, {varss});
    polySystem apply(flatten entries f.PolyMap, x -> phi(x))
    )
rationalToComplex RingElement := f -> (
    if (degree f)#0 > 0 then (k := sub((leadCoefficient f)*ii + leadCoefficient((f - leadTerm f)), CC);
    k)
    else (k = sub(leadCoefficient f, CC);
        k)
	)
rationalToComplex Point := f -> (
    point{apply(coordinates f, x -> rationalToComplex x)}
        )



computeConstants = method()
computeConstants(PolySystem, Point) := (ff, xx) -> (
--    R := ring ff;
--    symbolic := coefficientRing R =!= CC_53;
--    if symbolic then (f := rationalToComplex(ff);
--	x := rationalToComplex(xx));
--    numOfVari := numgens R; 
--    numOfPoly := # equations ff;
--    jacobianOfSys := sub(jacobian ff,R);
--    J := evaluate(jacobianOfSys, xx);
--    if det J == 0 then error "The Jacobian is not invertible";
--    eval := evaluate(ff,xx);
--    if symbolic then y := point(inverse J * eval)
--   else y = point (solve(J,eval));
--    degs := flatten for i from 1 to numOfPoly list degree ((equations ff)#(numOfPoly-i));
--    diagonals := flatten for i from 1 to numOfPoly list sqrt((degs)#(numOfPoly-i))*(oneNorm(xx))^((degs)#(numOfPoly-i)-1);
--    deltaD := sub(diagonalMatrix diagonals, coefficientRing R);
--    if symbolic then (mu := max {1, polySysNorm(f)*norm(solve(J,deltaD))})
--    else mu = max {1, polySysNorm(ff)*norm(solve(J,deltaD))};    
--    maxdeg := max degs;
--    beta := hermitianNorm(y);
--    gamma := mu*((maxdeg)^(3/2))*(1/2)*(1/oneNorm(xx));
--   alpha := beta * gamma;
--    (alpha, beta, gamma)
    R := ring ff;
    symbolic := coefficientRing R =!= CC_53;
    if symbolic then (numOfVari := numgens R; 
    	numOfPoly := # equations ff;
    	jacobianOfSys := jacobian ff;
    	J := evaluate(jacobianOfSys, xx);
    	if det J == 0 then error "The Jacobian is not invertible";
    	eval := evaluate(ff,xx);
    	y := point(inverse J * eval);
    	degs := flatten for i from 1 to numOfPoly list degree ((equations ff)#(numOfPoly-i));
    	diagonals := flatten for i from 1 to numOfPoly list sqrt((degs)#(numOfPoly-i))*(oneNorm(xx))^((degs)#(numOfPoly-i)-1);
    	deltaD := complexToRational(diagonalMatrix diagonals, coefficientRing R);
    	mu := max {1, complexToRational((polySysNorm(ff))^2,coefficientRing R)*complexToRational(frobeniusNormSq(inverse J * deltaD),coefficientRing R)};    
    	maxdeg := max degs;
    	beta := complexToRational(hermitianNorm(y), coefficientRing R);
    	gamma := mu*complexToRational(((maxdeg)^(3))*(1/4)*(1/oneNorm(xx))^2,coefficientRing R);
    	alpha := beta^2 * gamma;
    	(alpha, beta, gamma))
    else   ( numOfVari = numgens R; 
    	numOfPoly = # equations ff;
    	jacobianOfSys = sub(jacobian ff,R);
    	J = evaluate(jacobianOfSys, xx);
    	if det J == 0 then error "The Jacobian is not invertible";
    	eval = evaluate(ff,xx);
    	y = point (solve(J,eval));
    	degs = flatten for i from 1 to numOfPoly list degree ((equations ff)#(numOfPoly-i));
    	diagonals = flatten for i from 1 to numOfPoly list sqrt((degs)#(numOfPoly-i))*(oneNorm(xx))^((degs)#(numOfPoly-i)-1);
    	deltaD = sub(diagonalMatrix diagonals, coefficientRing R);
    	mu = max {1, polySysNorm(ff)*norm(solve(J,deltaD))};    
    	maxdeg = max degs;
    	beta = hermitianNorm(y);
    	gamma = mu*((maxdeg)^(3/2))*(1/2)*(1/oneNorm(xx));
    	alpha = beta^2 * gamma^2;
    	(alpha, beta, gamma))
    )








certifySolution = method()
certifySolution(PolySystem, Point) := (f, x) -> (
    Consts := computeConstants(f,x);
    if rationalToComplex(Consts #0)<((13-3*sqrt(17))/4)^2 then (
	 print "The point is an approximate solution to the system";
	 << "The value of alpha is " << sqrt(rationalToComplex((Consts)#0)) << endl;
	 << "The radius is " << 2*rationalToComplex((Consts)#1) << endl;
	 true
	 )
    else (
	print "The point is not an approximate solution to the system";
	false
	)
    )

certifyDistinctSoln = method()
certifyDistinctSoln(PolySystem, Point, Point) := (f, x1, x2) -> (
    Consts1 := computeConstants(f,x1);
    Consts2 := computeConstants(f,x2);
    if rationalToComplex(Consts1 #0) >= ((13-3*sqrt(17))/4)^2 then (
	print "The first point is not an approximate solution to the system";
	false
	)
    else if rationalToComplex(Consts2 #0) >= ((13-3*sqrt(17))/4) then (
	print "The second point is not an approximate solution to the system";
	false
	)
    else if hermitianNorm(point{(coordinates x1)-(coordinates x2)}) > 2*(rationalToComplex((Consts1)#1) + rationalToComplex((Consts2)#1)) then (
	print "Associated solutions are distinct";
	true
	)
    else if rationalToComplex((Consts1)#0) < (0.03)^2 and hermitianNorm(point{(coordinates x1)-(coordinates x2)})^2 < 1/(400*rationalToComplex((Consts1)#2)) or rationalToComplex((Consts2)#0) < 0.009 and hermitianNorm(point{(coordinates x1)-(coordinates x2)})^2 < 1/(400*rationalToComplex((Consts2)#2)) then (
	print "Associated solutions are not distinct";
	false
	)
    )


TEST ///
R = CC[x,y]
f = polySystem {x + y, x^2 - 4}
sols = solveSystem f
assert all(sols, p -> certifySolution(f,p))
p = point{{2.0_CC, -2.0}}
q = point{{-2_CC, 2.000001}}
computeConstants(f,p)
certifySolution(f,p)
certifyDistinctSoln(f,p,q)
FFF = QQ[j]/ideal(j^2+1)
pp = point {complexToRational(coordinates q, FFF)}
qq = point {complexToRational(coordinates p, FFF)}
R'=FFF[x,y]
ff = complexToRational(f,FFF)
certifySolution(ff,pp)
certifyDistinctSoln(ff,pp,qq)
///

end



restart
needsPackage "NumericalCertification"
FF = QQ[i]/ideal(i^2 + 1)
R = FF[x,y]
p1 = point matrix{{2,4+i}}
hermitianNorm(p1)
absValue(2)
absValue(4+i)


restart
check "NumericalCertification"
uninstallAllPackages()
installPackage "NumericalCertification"
viewHelp NumericalCertification
