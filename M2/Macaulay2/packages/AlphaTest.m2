newPackage(
	"AlphaTest",
    	Version => "0.5", 
    	Date => "July, 2016",
    	Authors => {
	     {Name => "Kisun Lee", Email => "klee669@gatech.edu"}
	     },
    	HomePage => "http://people.math.edu/~klee669",
    	Headline => "Alpha Test",
	PackageExports => {"NumericalAlgebraicGeometry"},
	AuxiliaryFiles => true, -- set to true if package comes with auxiliary files
    	DebuggingMode => true		 -- set to true only during development
    	)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists

export {"absValue", "hermitianNorm", "oneNorm", "polyNorm", "polySysNorm", "computeConstants", "certifySolutions", "certifyDistinctSoln"}
exportMutable {}

absValue = method()
absValue ZZ := abs
absValue(RingElement) := r -> (
    R := ring(r);
    LT := leadTerm(sub(r,R));
    VV := leadCoefficient(LT)^2 + (sub(r,R)-LT)^2;
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
	((abs c))^2*(product(e,b->b!)*(((degree r)#0-(sum e))!)/((degree r)#0)!)
 	))
    )

polySysNorm = method()
polySysNorm(PolySystem) := f -> (
    listOfEq := equations f;
    listOfpolyNorms := apply( listOfEq, i -> (polyNorm(i))^2);
    N := sum listOfpolyNorms
    )


computeConstants = method()
computeConstants(PolySystem, Point) := (f, x) -> (
    R := ring f;
    numOfVari := numgens R; 
    numOfPoly := # equations f;
    jacobianOfSys := sub(jacobian f,R);
    J := evaluate(jacobianOfSys, x);
    if det J == 0 then error "The Jacobian is not invertible";
    eval := evaluate(f,x);
    y := point solve(J, eval);
    degs := flatten for i from 1 to numOfPoly list degree ((equations f)#(numOfPoly-i));
    diagonals := flatten for i from 1 to numOfPoly list sqrt((degs)#(numOfPoly-i))*(oneNorm(x))^((degs)#(numOfPoly-i)-1);
    deltaD := sub(diagonalMatrix diagonals, CC);
    mu := max {1, polySysNorm(f)*norm(solve(J,deltaD))};
    maxdeg := max degs;
    beta := hermitianNorm(y);
    gamma := mu*((maxdeg)^(3/2))*(1/2)*(1/oneNorm(x));
    alpha := beta * gamma;
    (alpha, beta, gamma)
    )


certifySolutions = method()
certifySolutions(PolySystem, Point) := (f, x) -> (
    Consts := computeConstants(f,x);
    if (Consts #0)<((13-3*sqrt(17))/4) then (
	 print "The point is an approximate solution to the system";
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
    if (Consts1 #0) >= ((13-3*sqrt(17))/4) then (
	print "The first point is not an approximate solution to the system";
	false
	)
    else if (Consts2 #0) >= ((13-3*sqrt(17))/4) then (
	print "The second point is not an approximate solution to the system";
	false
	)
    else if hermitianNorm(point{(coordinates x1)-(coordinates x2)}) > 2*((Consts1)#1 + (Consts2)#1) then (
	print "Associated solutions are distinct";
	true
	)
    else if (Consts1)#0 < 0.03 and hermitianNorm(point{(coordinates x1)-(coordinates x2)}) < 1/(20*(Consts1)#2) or (Consts2)#0 < 0.03 and hermitianNorm(point{(coordinates x1)-(coordinates x2)}) < 1/(20*(Consts2)#2) then (
	print "Associated solutions are not distinct";
	false
	)
    )


TEST ///
R = CC[x,y]
f = polySystem {x + y, x^2 - 4}
sols = solveSystem f
assert all(sols, p -> certifySolutions(f,p))
p = point{{2.0, -2.0}}
q = point{{-2, 2.000001}}
certifySolutions(f,p)
certifyDistinctSoln(f,p,q)
///

end



restart
needsPackage "AlphaTest"
FF = QQ[i]/ideal(i^2 + 1)
R = FF[x,y]
p1 = point matrix{{2,4+i}}
hermitianNorm(p1)
absValue(2)
absValue(4+i)

