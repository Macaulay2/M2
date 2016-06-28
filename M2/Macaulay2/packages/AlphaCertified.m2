restart
needsPackage "NumericalAlgebraicGeometry"

R = CC[x,y]

HermitianNorm = method()
HermitianNorm(Point) := x -> (
    N := sqrt(sum(apply(coordinates x, i -> abs(i)^2)))
    )

OneNorm = method()
OneNorm(Point) := x -> (
    N := sqrt(1+(HermitianNorm(x)*HermitianNorm(x)))
    )

PolyNorm = method()
PolyNorm(RingElement) := r -> (
    L = listForm r;
    sum(L,a->(
	(e,c) := a;
	((abs c))^2*(product(e,b->b!)*(((degree r)#0-(sum e))!)/((degree r)#0)!)
 	))
    )

PolySysNorm = method()
PolySysNorm(PolySystem) := f -> (
    listOfEq := equations f;
    listOfPolyNorms := apply( listOfEq, i -> (PolyNorm(i))^2);
    N := sum listOfPolyNorms
    )


ComputeConstants = method()
ComputeConstants(PolySystem, Point) := (f, x) -> (
    numOfVari := numgens ring f; 
    numOfPoly := # equations f;
    jacobianOfSys := sub(jacobian f,R);
    J := evaluate(jacobianOfSys, x);
    if det J == 0 then error "The Jacobian is not invertible";
    eval := evaluate(f,x);
    y := point solve(J, eval);
    degs := flatten for i from 1 to numOfPoly list degree ((equations f)#(numOfPoly-i));
    diagonals := flatten for i from 1 to numOfPoly list sqrt((degs)#(numOfPoly-i))*(OneNorm(x))^((degs)#(numOfPoly-i)-1);
    deltaD := sub(diagonalMatrix diagonals, CC);
    mu := max {1, PolySysNorm(f)*norm(solve(J,deltaD))};
    maxdeg := max degs;
    beta := HermitianNorm(y);
    gamma := mu*((maxdeg)^(3/2))*(1/2)*(1/OneNorm(x));
    alpha := beta * gamma;
    (alpha, beta, gamma)
    )

CertifySolns = method()
CertifySolns(PolySystem, Point) := (f, x) -> (
    Consts := ComputeConstants(f,x);
    if (Consts #0)<((13-3*sqrt(17))/4) then print "The point is an approximate solution to the system"
    else print "The point is not an approximate solution to the system"
    )

CertifyDistinctSoln = method()
CertifyDistinctSoln(PolySystem, Point, Point) := (f, x1, x2) -> (
    Consts1 := ComputeConstants(f,x1);
    Consts2 := ComputeConstants(f,x2);
    if (Consts1 #0) >= ((13-3*sqrt(17))/4) then print "The first point is not an approximate solution to the system";
    if (Consts2 #0) >= ((13-3*sqrt(17))/4) then print "The second point is not an approximate solution to the system";
    if HermitianNorm(point{(coordinates x1)-(coordinates x2)}) > 2*((Consts1)#1 + (Consts2)#2) then print "Associated solutions are distinct";
    if (Consts1)#0 < 0.03 and HermitianNorm(point{(coordinates x1)-(coordinates x2)}) < 1/(20*(Consts1)#2) or (Consts2)#0 < 0.03 and HermitianNorm(point{(coordinates x1)-(coordinates x2)}) < 1/(20*(Consts2)#2) then print "Associated solutions are not distinct";
    )

f = polySystem {x + y, x - 4}
p = point{{10.0000011, -4.9991}}
q = point{{10.00011, -4.991+ii}}
ComputeConstants(f,p)
CertifySolns(f,p)
CertifyDistinctSoln(f,p,q)
