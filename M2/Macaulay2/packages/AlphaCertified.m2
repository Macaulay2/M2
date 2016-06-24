restart
needsPackage "NumericalAlgebraicGeometry"

help PolySystem
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
    norm r
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
    Consts := (alpha, beta, gamma)
    )


f = polySystem {x + y, x - 4}
p = point{{1+3*ii, 2.3+ii}}
ComputeConstants(f,p)
