newPackage(
	"NumericalCertification",
    	Version => "1.5",
    	Date => "May, 2022",
    	Authors => {
	     {Name => "Kisun Lee", Email => "kil004@ucsd.edu",     	HomePage => "https://klee669.github.io"},
     	     {Name => "Special thanks: Michael Burr, Anton Leykin, Thomas Yahl"}
	     },
    	Headline => "numerical certification",
	PackageExports => {"NumericalAlgebraicGeometry"},
	Configuration => {"ALPHACERTIFIEDexec" => "alphaCertified"},
    	DebuggingMode => true,		 -- set to true only during development
    	--DebuggingMode => false,
	AuxiliaryFiles => true
    	)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists

export {
    "computeConstants",
    "certifyRegularSolution",
    "certifyDistinctSolutions",
    "certifyRealSolution",
    "alphaTheoryCertification",
    "certifySolutions",
    "CCi",
    "CCiMatrix",
    "intervalCCi",
    "matrixCCi",
    "midpointCCi",
    "pointToMatrix",
    "krawczykOperator",
    "krawczykTest",
    "krawczykRealnessTest",
    "certifySingularSolution",
    "CertificationOptions",
    "alphaTheory",
    "intervalArithmetic",
    "alphaCertified",
    "ALGORITHM",
    "ARITHMETICTYPE",
    "PRECISION",
    "REFINEDIGITS",
    "NUMRANDOMSYSTEMS",
    "RANDOMDIGITS",
    "RANDOMSEED",
    "NEWTONONLY",
    "NUMITERATIONS",
    "REALITYCHECK",
    "REALITYTEST"
    }
exportMutable {}



ALPHACERTIFIEDexe=(options NumericalCertification).Configuration#"ALPHACERTIFIEDexec"



conjugateGaussian = method()
conjugateGaussian(RingElement) := x -> (
    R := ring x;
    var := first gens R;
    x - 2* var * coefficient(var, x)
    )

conjugateGaussianRationalMatrix = method()
conjugateGaussianRationalMatrix(Matrix) := m -> (
    matrix apply(entries m, k -> apply(k, i -> conjugateGaussian i))
    )


sqabsForGaussianRational = method()
sqabsForGaussianRational(RingElement) := x -> (
    R := ring x;
    var := first gens R;
    rationalRing := coefficientRing R;
    sub((coefficient(var, x))^2, rationalRing) + sub((x - var * coefficient(var, x))^2, rationalRing)
    )



pointNorm = method()
pointNorm(Point) := x -> (
    pointNorm matrix x
    )
pointNorm(Matrix) := x -> (
    coordinateList := flatten entries x;
    -- returns the square of the norm
    R := ring first coordinateList;
    if precision R =!= infinity then (
    	1 + sum(apply(coordinateList, i -> abs(i)^2))
	)
    else if R =!= QQ then (
	var := first gens R;
	rationalRing := coefficientRing R;
	1 + sum apply(coordinateList, i -> sqabsForGaussianRational i)
	)
    else (
    	1 + sum(apply(coordinateList, i -> i^2))
	)
    )



polyNorm = method()
polyNorm(Number) := r -> (
    abs(r)^2
    )
polyNorm(RingElement) := r -> (
    R := coefficientRing ring r;
    L := listForm r;
    if precision R =!= infinity then (
	sum(L,a->(
		(e,c) := a;
		abs(c)^2*(product(e,b->b!)*(((degree r)#0-(sum e))!)/((degree r)#0)!)
		)
	)
    )
    else if R =!= QQ then (
	var := first gens R;
	rationalRing := coefficientRing R;
    	sum(L,a->(
	(e,c) := a;
	    (sqabsForGaussianRational c)*(product(e,b->b!)*(((degree r)#0-(sum e))!)/((degree r)#0)!)
 	))
	)
    else (
	sum(L,a->(
		(e,c) := a;
		(c^2)*(product(e,b->b!)*(((degree r)#0-(sum e))!)/((degree r)#0)!)
		)
	)
	)
    )





polySysNorm = method()
polySysNorm(PolySystem) := f -> (
    listOfEq := equations f;
    listOfpolyNorms := apply(listOfEq, i -> polyNorm(i));
    N := sum listOfpolyNorms
    )


newtonOper = method()
newtonOper(PolySystem, Point) := (f, x) -> (
    newtonOper(f, matrix x)
    )
newtonOper(PolySystem, Matrix) := (f, x) -> (
    jacOfPoly := jacobian f;
    evalJac := evaluate(jacOfPoly, x);
    inverseOfJac := inverse(evalJac);
    evalSys := evaluate(f, x);
    point {transpose x - inverseOfJac * evalSys}
    )



computeConstants = method() --computes alpha^2 beta^2 gamma^2
computeConstants(PolySystem, Matrix) := (f, x) -> (
    computeConstants(f, point x)
    )
computeConstants(PolySystem, Point) := (f, x) -> (
    eqs := equations f;
    J := evaluate(jacobian f, x);
    inverseJ := inverse J;
    R := coefficientRing ring f;
    pointNormx := pointNorm x;
    degs := select(flatten apply(eqs, i -> degree i), i -> i =!= 0);
    if precision R =!= infinity then (
	R = R;
    	if det J == 0 then error "The Jacobian is not invertible";
    	-- beta
    	y := point(inverseJ * evaluate(f,x));
    	beta := sub(sum apply(y#Coordinates, i -> abs(i)^2),R);
    	deltaD := diagonalMatrix flatten apply(degs, i -> sqrt(i * (pointNormx)^(i-1)));
     	mu := max {1, polySysNorm(f) * (norm(2,inverseJ * deltaD))^2};
	)
    else if R =!= QQ then (
	var := first gens R;
	R = coefficientRing R;
	print "Warning: invertibility check for Jacobian is skipped for Gaussian rational inputs";
    	-- beta
    	y = point(inverseJ * evaluate(f,x));
    	beta = sub(sum apply(y#Coordinates, i -> sqabsForGaussianRational i),R);
    	deltaD = diagonalMatrix flatten apply(degs, i -> i * (pointNormx)^(i-1));
	sqFrobenius := trace(inverseJ * deltaD * (transpose conjugateGaussianRationalMatrix inverseJ));
     	mu = max {1, polySysNorm(f) * (sub((coefficient(var, sqFrobenius))^2, R) + sub((sqFrobenius - var * coefficient(var,sqFrobenius)), R))};
	)
    else (
	R = R;
    	if det J == 0 then error "The Jacobian is not invertible";
    	-- beta
    	y = point(inverseJ * evaluate(f,x));
    	beta = sub(sum apply(y#Coordinates, i -> i^2),R);
    	deltaD = diagonalMatrix flatten apply(degs, i -> i * (pointNormx)^(i-1));
     	mu = max {1, polySysNorm(f) * trace(inverseJ * deltaD * (transpose inverseJ))};
	);
    -- gamma
    gamma := sub(mu*((max degs)^3)/(4* pointNormx), R);
    alpha := sub(beta * gamma, R);
    (alpha, beta, gamma)
    )


certifyRegularSolution = method() -- returns null if not successful, (alpha,beta,gamma) if alpha-certified
certifyRegularSolution(PolySystem, Matrix) := (f, x) -> (
    certifyRegularSolution(f, point x)
    )
certifyRegularSolution(PolySystem, Point) := (f, x) -> (
    alpha := first computeConstants(f,x);
    -- check: alpha < (13-3*sqrt(17))/4
    if 16*alpha < 169 and (322-16*alpha)^2 > 78*78*17 then true else false
    )
certifyRegularSolution(PolySystem, List) := (f, L) -> (
    apply(L, i -> certifyRegularSolution(f, i))
    )

certifyDistinctSolutions = method()
certifyDistinctSolutions(PolySystem, Matrix, Matrix) := (f, x1, x2) -> (
    certifyDistinctSolutions(f, point x1, point x2)
    )
certifyDistinctSolutions(PolySystem, Point, Point) := (f, x1, x2) -> (
    R := coefficientRing ring f;
    if precision R =!= infinity then (
	R = R;
	)
    else if R =!= QQ then (
	R = coefficientRing R;
	)
    else (
 	R = R;
	);
    Consts1 := computeConstants(f,x1);
    Consts2 := computeConstants(f,x2);
    if precision R =!= infinity then (
    	normOfDist := (norm(2,point{(coordinates x1)-(coordinates x2)}))^2;
	)
    else (
    	normOfDist = sum apply((point{(coordinates x1)-(coordinates x2)})#Coordinates, c->sub(c^2,R));
	);
    if Consts1 #0 >= ((13-3*sqrt(17))/4)^2 or Consts2 #0 >= ((13-3*sqrt(17))/4)^2 then (
	false
	)
    else if normOfDist > 4*((Consts1)#1 + (Consts2)#1 + 2*sqrt((Consts1)#1 * (Consts2)#1)) then (
	true
	)
    else if (Consts1)#0 < 9/10000 and normOfDist < 1/(400*(Consts1)#2) or (Consts2)#0 < 9/10000 and normOfDist < 1/(400*(Consts2)#2) then (
	false
	)
    else (
      	false
	)
    )


certifyRealSolution = method()
certifyRealSolution(PolySystem, Matrix) := (f, x) -> (
    certifyRealSolution(f, point x)
    )
certifyRealSolution(PolySystem, Point) := (f, x) -> (
    (alpha, beta, gamma) := computeConstants(f,x);
    R := coefficientRing ring f;
    if precision R =!= infinity then (
    	imagPart := apply(coordinates x, i -> imaginaryPart(i));
	R = R;
	)
    else if R =!= QQ then (
	coordinatesOfx := coordinates x;
	l := length coordinatesOfx;
	imagPart = {};
	for i from 0 to l-1 do if degree(coordinatesOfx#i) === 1 then (
		append(imagPart,leadCoefficient sub(i,R));
		)
	    else (
		append(imagPart, 0);
		);
	R = coefficientRing R;
	)
    else (
    	imagPart = apply(coordinates x, i -> imaginaryPart(i));
	R = R;
	);
    normOfimagPart := sum apply(imagPart, i -> sub(i^2,R));
    if (normOfimagPart > 4*beta) then false
    else if alpha < 9/10000 and normOfimagPart < 1/(400*gamma) then true
    else (
--	print "apply more Newton's operators!";
    false
    )
    )

alphaTheoryCertification = method()
alphaTheoryCertification(PolySystem, List) := (f, X) -> (
    R := coefficientRing ring f;
    if precision R =!= infinity then (
	R = R;
	)
    else if R =!= QQ then (
	R = coefficientRing R;
	)
    else (
 	R = R;
	);
    Y := select(X, i->certifyRegularSolution(f,i)=!=false);
    C := apply(X, i-> first computeConstants(f,i)); -- Can we have this without using function twice?
    S := new MutableList from Y;
    for i from 0 to length(Y) - 1 do S#i = true;
    for i from 0 to length(Y) - 2 do for j from i+1 to length(Y) - 1 do if (
	S#i == true and S#j == true
	)
    then (
	S#j = certifyDistinctSolutions(f,Y#i, Y#j);
	);
    D := {};
    for i from 0 to length(Y) - 1 do if S#i == true then D = append(D, Y#i);
    Real := {};
    if R =!= CC then for i from 0 to length(D) - 1 do if certifyRealSolution(f,D#i) == true then Real = append(Real,D#i);
    new MutableHashTable from {"certifiedSolutions" => Y, "alphaValues" => C, "certifiedDistinct" =>D, "certifiedReal" => Real}
    )










-- a function converting a polynomial into alphaCertified input format.
-- input : polynomial
-- output : the first line is the number of terms of an input.
--    	    the second to last lines represent terms of an input polynomial in a way that
--    	    the first (variable-many) numbers are  degrees of each variable in the monomial and
--          the last two numbers are real and imaginary parts of its coefficient
degCoeff = method()
degCoeff(RingElement) := f -> (
    variables := gens ring f;
    R := coefficientRing ring f;
    if precision R == infinity then (
	print "error! Use a polynomial ring with coefficients CC or RR"; break
	);
    (E, C) := coefficients f;
    E = flatten entries E;
    C = flatten entries C;
    strList := apply(length E, i ->
        replace("[{,},,]", "", toString(apply(variables, j ->
	    degree(j, E#i)) | {lift(realPart sub(C#i,CC), QQ), lift(imaginaryPart sub(C#i,CC), QQ)}))
	);
    prepend(length E, strList)
    )





-- a function converting a polynomial system into alphaCertified input format.
-- input : polynomial system
-- output : a directory to a temporary file which can be used as an input for alphaCertified.
--          the first line consists of the number of variables and the number of polynomials.
--    	    each block represents information about each polynomial in the system.
toACertifiedPoly = method()
toACertifiedPoly(PolySystem) := P -> (
    fn := temporaryFileName();
    (numOfVars, numOfPolys) := (P.NumberOfVariables, P.NumberOfPolys);
    fn << toString numOfVars | " " | toString numOfPolys << endl;
    fn << "" << endl;
    polyList := flatten entries P.PolyMap;
    strList := apply(polyList, i ->
	degCoeff i);
    apply(flatten strList, i -> fn << i << endl);
    fn << close;
    fn
    )



-- a function converting a point into a block of digits for alphaCertified input.
-- input : Matrix representing a coordinate of a point or Point
-- output : a list of coordinates of a given point.
pointBlock = method()
pointBlock(Point) := P -> (
    pointBlock matrix P
    )
pointBlock(Matrix) := M -> (
    strList := apply(flatten entries M, i ->
        replace("[{,},,]", "", toString({lift((realPart i)_QQ,QQ), lift((imaginaryPart i)_QQ,QQ)}))
	);
    strList = append(strList, "")
    )



toACertifiedPoint = method()
toACertifiedPoint(List) := L -> (
    fn := temporaryFileName();
    n := length L;
    fn << toString n << endl;
    fn << "" << endl;
    apply(L, i ->
	apply(pointBlock i, j ->
	    fn << j << endl)
	);
    fn << close;
    fn
    )

alphaCertified = method(Options => {
	ALGORITHM => 2,
	ARITHMETICTYPE => 0,
	PRECISION => 96,
	REFINEDIGITS => 0,
	NUMRANDOMSYSTEMS => 2,
	RANDOMDIGITS => 10,
	RANDOMSEED => random 10000,
	NEWTONONLY => 0,
	NUMITERATIONS => 2,
	REALITYCHECK => 1,
	REALITYTEST => 0
	}
	)
alphaCertified(PolySystem, List) := o -> (P, L) -> (
    fin1 := toACertifiedPoly P;
    fin2 := toACertifiedPoint L;
    fin3 := temporaryFileName();
    apply(# o, i -> fin3 << toString(keys o)#i | ": "|toString(values o)#i|";" << endl);
    fin3 << close;
    run("cd " | ALPHACERTIFIEDexe |"; ./alphaCertified " | fin1 |" "| fin2 |" " | fin3);
    )
alphaCertified(List, List) := (P, L) -> alphaCertified(polySystem P, L)
alphaCertified(Ideal, List) := (P, L) -> alphaCertified(polySystem P, L)






certifySolutions = method(Options => {
	Strategy => "alphaTheory"})
certifySolutions(PolySystem, List, Number) := opts -> (P, L, iL) -> (
    if opts.Strategy == "alphaTheory" then return certifySolutionsAlpha(P,L,iL)
    else if opts.Strategy == "intervalArithmetic" then return certifySolutionsInterval(P,L,iL)
    else if opts.Strategy == "alphaCertified" then return alphaCertified(P,L)
    else error "Unknown strategy";
    )
certifySolutions(PolySystem, List) := opts -> (P, L) -> (
    if opts.Strategy == "alphaTheory" then return certifySolutionsAlpha(P,L)
    else if opts.Strategy == "intervalArithmetic" then return certifySolutionsInterval(P,L)
    else if opts.Strategy == "alphaCertified" then return alphaCertified(P,L)
    else error "Unknown strategy";
    )
certifySolutions(Ideal, List, Number) := opts -> (P, L, iL) -> (
    if opts.Strategy == "alphaTheory" then return certifySolutionsAlpha(polySystem P,L,iL)
    else if opts.Strategy == "intervalArithmetic" then return certifySolutionsInterval(polySystem P,L,iL)
    else if opts.Strategy == "alphaCertified" then return alphaCertified(polySystem P,L)
    else error "Unknown strategy";
    )
certifySolutions(Ideal, List) := opts -> (P, L) -> (
    if opts.Strategy == "alphaTheory" then return certifySolutionsAlpha(polySystem P,L)
    else if opts.Strategy == "intervalArithmetic" then return certifySolutionsInterval(polySystem P,L)
    else if opts.Strategy == "alphaCertified" then return alphaCertified(polySystem P,L)
    else error "Unknown strategy";
    )
certifySolutions(List, List, Number) := opts -> (P, L, iL) -> (
    if opts.Strategy == "alphaTheory" then return certifySolutionsAlpha(polySystem P,L,iL)
    else if opts.Strategy == "intervalArithmetic" then return certifySolutionsInterval(polySystem P,L,iL)
    else if opts.Strategy == "alphaCertified" then return alphaCertified(polySystem P,L)
    else error "Unknown strategy";
    )
certifySolutions(List, List) := opts -> (P, L) -> (
    if opts.Strategy == "alphaTheory" then return certifySolutionsAlpha(polySystem P,L)
    else if opts.Strategy == "intervalArithmetic" then return certifySolutionsInterval(polySystem P,L)
    else if opts.Strategy == "alphaCertified" then return alphaCertified(polySystem P,L)
    else error "Unknown strategy";
    )


certifySolutionsAlpha = method()
certifySolutionsAlpha(PolySystem, List, Number) := (P, L, iL) -> (
    regSolutions := delete( ,apply(L, i -> if certifyRegularSolution(P, i) then i else null));
    apply(regSolutions, i -> L = delete(i, L));
    multSolutions := delete( ,apply(L, i -> (
	    iter := 1;
	    c := certifySingularSolution(P, i, iter,Strategy => "alphaTheory");
	    while c == false and iter <= iL do (
		iter = iter + 1;
	    	c = certifySingularSolution(P, i, iter,Strategy => "alphaTheory");
		);
	    if c == true then i else null
	    )));
    apply(multSolutions, i -> L = delete(i, L));
    ac := alphaTheoryCertification(P, regSolutions);
    new MutableHashTable from {"certifiedRegularSolutions" => ac#"certifiedSolutions",
	 "alphaValues" => ac#"alphaValues",
	 "certifiedDistinct" => ac#"certifiedDistinct",
	 "certifiedReal" => ac#"certifiedReal",
	 "certifiedSingularSolutions" => multSolutions,
	 "nonCertifiedSolutions" => L}
    )
certifySolutionsAlpha(PolySystem, List) := (P, L) -> (
    regSolutions := delete( ,apply(L, i -> if certifyRegularSolution(P, i) then i else null));
    apply(regSolutions, i -> L = delete(i, L));
    multSolutions := delete( ,apply(L, i -> (
	    c := certifySingularSolution(P, i, Strategy => "alphaTheory");
	    if c == true then i else null
	    )));
    apply(multSolutions, i -> L = delete(i, L));
    ac := alphaTheoryCertification(P, regSolutions);
    new MutableHashTable from {"certifiedRegularSolutions" => ac#"certifiedSolutions",
	 "alphaValues" => ac#"alphaValues",
	 "certifiedDistinct" => ac#"certifiedDistinct",
	 "certifiedReal" => ac#"certifiedReal",
	 "certifiedSingularSolutions" => multSolutions,
	 "nonCertifiedSolutions" => L}
    )


certifySolutionsInterval = method()
certifySolutionsInterval(PolySystem, List, Number) := (P, I, iL) -> (
    regSolutions := delete( ,apply(#I, i -> if krawczykTest(P, I#i) then I#i));
    apply(regSolutions, i -> I = delete(i, I));
    multSolutions := delete( ,apply(#I, i -> (
	    iter := 1;
	    c := certifySingularSolution(P, I#i, iter, Strategy => "intervalArithmetic");
	    while c == false and iter <= iL do (
		iter = iter + 1;
	    	c = certifySingularSolution(P, I#i, iter, Strategy => "intervalArithmetic");
		);
	    if c == true then I#i else null
	    )));
    apply(multSolutions, i -> I = delete(i, I));
    new MutableHashTable from {"certifiedRegularSolutions" => apply(regSolutions, i ->
	    krawczykOperator(P, i)),
	 "certifiedReal" => delete( ,apply(regSolutions, i ->
	     if krawczykRealnessTest(P, i) then krawczykOperator(P, i) else null)),
	 "certifiedSingularSolutions" => multSolutions,
	 "nonCertifiedSolutions" => I}
    )
certifySolutionsInterval(PolySystem, List) := (P, I) -> (
    regSolutions := delete( ,apply(#I, i -> if krawczykTest(P, I#i) then I#i));
    apply(regSolutions, i -> I = delete(i, I));
    multSolutions := delete( ,apply(#I, i -> (
	    c := certifySingularSolution(P, I#i, Strategy => "intervalArithmetic");
	    if c == true then I#i else null
	    )));
    apply(multSolutions, i -> I = delete(i, I));
    new MutableHashTable from {"certifiedRegularSolutions" => apply(regSolutions, i ->
	    krawczykOperator(P, i)),
	 "certifiedReal" => delete( ,apply(regSolutions, i ->
	     if krawczykRealnessTest(P, i) then krawczykOperator(P, i) else null)),
	 "certifiedSingularSolutions" => multSolutions,
	 "nonCertifiedSolutions" => I}
    )


----- define CCi
----------------
CCi = new Type of List
CCiMatrix = new Type of List
net CCi := i -> net first i | " + " | net last i | "*ii"
net CCiMatrix := i -> (
    nr := length i;
    nc := length first i;
    t := net "";
    td := net "";
    t1 := net "| ";
    for k from 0 to nc - 1 do (
	d := i#0#k;
	t1 = net t1 | net " " | net d;
	);
    t1 = net t1 | net " |";
    td = net td | net t1;
    for j from 1 to nr - 1 do (
	t = net "| ";
	for k from 0 to nc - 1 do (
	    d := i#j#k;
	    t = net t | net " " | net d ;
	    );
	t = net t | net " |";
	td = net td || net t ;
        );
    td
    )

entries CCiMatrix := M -> (
    nr := length M;
    nc := length first M;
    apply(nr, i -> apply(nc, j -> M#i#j))
    )

flatten CCi := I -> (
    I
    )

isSubset(CCi, CCi) := (I1, I2) -> (
    reI1 := realPart I1;
    imI1 := imaginaryPart I1;
    reI2 := realPart I2;
    imI2 := imaginaryPart I2;
    all({isSubset(reI1, reI2), isSubset(imI1,imI2)}, i -> i == true)
    )

intervalCCi = method(TypicalValue => CCi)
intervalCCi (Number, Number) := (re, im) -> new CCi from (
    {interval re, interval im}
    )
intervalCCi (Number, RRi) := (re, im) -> new CCi from (
    {interval re, im}
    )
intervalCCi (RRi, Number) := (re, im) -> new CCi from (
    {re, interval im}
    )
intervalCCi (RRi, RRi) := (re, im) -> new CCi from (
    {re, im}
    )
intervalCCi Number := n -> new CCi from (
    re := realPart n;
    im := imaginaryPart n;
    intervalCCi(re, im)
    )
intervalCCi RRi := re -> new CCi from (
    intervalCCi(re, 0)
    )
intervalCCi CCi := i -> new CCi from (
    i
    )


CCi + CCi := (i1, i2) -> (
    intervalCCi(i1#0 + i2#0, i1#1 + i2#1)
    )
CCi * CCi := (i1, i2) -> (
    intervalCCi(i1#0*i2#0 - i1#1*i2#1, i1#0*i2#1 + i1#1*i2#0)
    )
Number * CCi := (n, I) -> (
    i1 := I#0;
    i2 := I#1;
    ren := realPart n;
    imn := imaginaryPart n;
    intervalCCi(ren * i1 - imn * i2, ren * i2 + imn * i1)
    )
CCi * Number := (I, n) -> (
    n * I
    )
CCi + Number := (I, n) -> (
    i1 := I#0;
    i2 := I#1;
    ren := realPart n;
    imn := imaginaryPart n;
    intervalCCi(i1 + ren, i2 + imn)
    )
Number + CCi := (n, I) -> (
    I + n
    )
CCi - Number := (I, n) -> (
    I + (-n)
    )
Number - CCi := (n, I) -> (
    -I + n
    )
CCi ^ Number := (I, n) -> (
    if n == 0 then return intervalCCi(interval(1,1),interval(0,0))
    else if n == 1 then return I
    else (
	j := I;
	k := 1;
	l := while k < n list j*I do (k = k+1; j = j*I);
	);
    last l
    )
CCi / CCi := (i1, i2) -> (
    X := i1#0;
    Y := i1#1;
    W := i2#0;
    Z := i2#1;
    intervalCCi((X*W + Y*Z)/(W*W + Z*Z), (Y*W  - X*Z)/(W*W + Z*Z))
    )

intersect(RRi, CCi) := (i1, i2) -> intervalCCi(intersect(i1, realPart i2), imaginaryPart i2)
intersect(CCi, RRi) := (i1, i2) -> intervalCCi(intersect(i2, realPart i1), imaginaryPart i1)
intersect(CCi, CCi) := (i1, i2) -> intervalCCi(intersect(realPart i2, realPart i1), intersect(imaginaryPart i1, imaginaryPart i2))

isEmpty CCi := i1 -> (isEmpty realPart i1 or isEmpty imaginaryPart i1)




matrixCCi = method(TypicalValue => CCiMatrix)
matrixCCi (Matrix, Matrix) := (re, im) -> new CCiMatrix from (
    if re*0 =!= im*0 then error "The real part and the imaginary part of the input must be the same size";
    assert all(flatten entries re, L -> class L === RRi);
    assert all(flatten entries im, L -> class L === RRi);
    nr := numrows re;
    nc := numcols re;
    reEntries := entries re;
    imEntries := entries im;
    apply(nr, i -> apply(nc, j -> intervalCCi(reEntries#i#j,imEntries#i#j)))
    )
matrixCCi List := m -> new CCiMatrix from (
           if #m === 0 then error "expected nonempty list";
           mm := apply(splice m,splice);
           if #mm === 0 then error "expected nonempty list";
           types := unique apply(mm,class);
           if #types === 1 then (
                type := types#0;
                if instance(type,Module) then matrix { apply(mm, v -> new Matrix from v) }
                else if ancestor(List, type) then (
                     if isTable mm then (matrixTable m)(mm)
                     else error "expected rows all to be the same length"
                     )
                else error "expected a table of ring elements or matrices, or a list of elements of the same module")
           else error "expected a table of ring elements or matrices, or a list of elements of the same module"
	   )
matrixCCi Point := i -> new CCi from (
    coord := coordinates i;
    return matrixCCi {coord/(j -> intervalCCi j)}
    )


matrixTable = method()
matrixTable List := opts -> (f) -> (
     types := unique apply(flatten f, class);
     f = apply(f, row -> new MutableList from row);
     m := #f;
     n := #f#0;
     tars := new MutableHashTable;
     srcs := new MutableHashTable;
     scan(m, row -> scan(n, col-> (
		    r := f#row#col;
		    if instance(r, Matrix) then (
			 if tars#?row then (
			      if tars#row != target r then error "expected matrices in the same row to have equal targets";
			      )
			 else tars#row = target r;
			 if srcs#?col then (
			      if srcs#col != source r then error "expected matrices in the same column to have equal sources";
			      )
			 else srcs#col = source r;
			 ))));
     f = toList \ f;
     f
     )

realPart CCi := I -> (
    first I
    )
imaginaryPart CCi := I -> (
    last I
    )

norm CCi := I -> (
    reI := realPart I;
    imI := imaginaryPart I;
    reN := max {left reI, right reI};
    imN := max {left imI, right imI};
    norm(reN + ii*imN)
    )
norm CCiMatrix := M -> (
    nr := length M;
    nc := length first M;
    max apply(nr, i -> sum apply(nc, j -> norm M#i#j))
    )

Number * CCiMatrix := (n, I) -> (
    i1 := I#0;
    i2 := I#1;
    ren := realPart n;
    imn := imaginaryPart n;
    matrixCCi(ren * i1 - imn * i2, ren * i2 + imn * i1)
    )
CCiMatrix * Number := (I, n) -> (
    n * I
    )
CCiMatrix ^ Number := (I, n) -> (
    nr := length first I;
    if n == 0 then return matrixCCi(id_(RRi^nr), id_(RRi^nr))
    else if n == 1 then return I
    else (
	j := I;
	k := 1;
	l := while k < n list j*I do (k = k+1; j = j*I);
	);
    last l
    )

transpose CCiMatrix := M -> (
    nr := length first M;
    nc := length M;
    matrixCCi apply(nr, i -> apply(nc, j -> M#j#i))
    )

numcols CCiMatrix := M -> length first M
numrows CCiMatrix := M -> length M

sub(RingElement, CCiMatrix) := (f, M) -> (
    if first degree f == 0 then return intervalCCi(interval(1,1),interval 0)
    else (
    realM := matrix apply(M, i -> apply(i, j -> first j));
    imagM := matrix apply(M, i -> apply(i, j -> last j));
    R := ring f;
    listFormf := apply(listForm f, i -> (sum first i, last i));
    termsOff := terms f;
    posrealf := sum apply(termsOff_(positions(listFormf, i -> (first i)%4 == 0)), j -> sub(j, imagM));
    negrealf := sum apply((-1)*termsOff_(positions(listFormf, i -> (first i)%4 == 2)), j -> sub(j, imagM));
    posimagf := sum apply(termsOff_(positions(listFormf, i -> (first i)%4 == 1)), j -> sub(j, imagM));
    negimagf := sum apply((-1)*termsOff_(positions(listFormf, i -> (first i)%4 == 3)), j -> sub(j, imagM));
    evalRealInterval := sub(f, realM) + posrealf + negrealf;
    evalImagInterval := posimagf + negimagf;
    intervalCCi(evalRealInterval,evalImagInterval))
    )

midpointCCi = method()
midpointCCi CCi := I -> (
    realI := I#0;
    imagI := I#1;
    (midpoint realI) + ii * (midpoint imagI)
    )


CCiMatrix * CCiMatrix := (l, n) -> (
    if length first l =!= length n then error "maps not composable";
    numrow := length l;
    numc := length(l#0);
    numcol := length(n#0);
    mat :=  apply(apply(l, i -> (k := 0; while k < numc list ( j := 0; while j < numcol list ((i#k)*((n#k)#j)) do j = j+1)  do k=k+1)), b -> sum b);
    mat
    )
Matrix * CCiMatrix := (l, n) -> (
    l = entries l;
    if length first l =!= length n then error "maps not composable";
    numrow := length l;
    numc := length(l#0);
    numcol := length(n#0);
    mat :=  apply(apply(l, i -> (k := 0; while k < numc list ( j := 0; while j < numcol list ((i#k)*((n#k)#j)) do j = j+1)  do k=k+1)), b -> sum b);
    matrixCCi mat
    )

Matrix + CCiMatrix := (l, n) -> (
    l = entries l;
    if length first l =!= length first n or length l =!= length n then error "matrices have different shapes";
    nr := length first l;
    nc := length l;
    matrixCCi apply(nc, j -> apply(nr, i -> l#j#i + n#j#i))
    )
Matrix - CCiMatrix := (l, n) -> (
    l + (-n)
    )
CCiMatrix + Matrix := (l, n) -> (
    n + l
    )
CCiMatrix - Matrix := (l, n) -> (
    -n + l
    )
CCiMatrix + CCiMatrix := (l, n) -> (
    if length first l =!= length first n or length l =!= length n then error "matrices have different shapes";
    nr := length first l;
    nc := length l;
    matrixCCi apply(nc, j -> apply(nr, i -> l#j#i + n#j#i))
    )
CCiMatrix - CCiMatrix := (l, n) -> (
    -n + l
    )


subOnTerm = method()
subOnTerm(Number, Matrix) := (f, ab) -> (
    interval(f,f)
    )
subOnTerm(RingElement, Matrix) := (f, ab) -> (
    R := ring f;
    gensR := gens R;
    if length gensR =!= numcols ab then error "encountered values for different number of variables";
    if class coefficientRing R === ComplexField then return subOnTermCC(f, ab);
    if f == 0 then return interval 0;
    c := sub(first flatten entries last coefficients f, coefficientRing R);
    b := flatten entries ab;
    c * (product apply(length gensR, i -> (b#i)^(degree(gensR#i, f))))
    )
subOnTerm(RingElement, CCiMatrix) := (f, ab) -> (
    R := ring f;
    gensR := gens R;
    if length gensR =!= numcols ab then error "encountered values for different number of variables";
    c := sub(first flatten entries last coefficients f, coefficientRing R);
    b := flatten entries ab;
    c * (product apply(length gensR, i -> (b#i)^(degree(gensR#i, f))))
    )


subOnTermCC = method()
subOnTermCC(RingElement, Matrix) := (f, ab) -> (
    if f == 0 then return intervalCCi 0;
    R := ring f;
    gensR := gens R;
    c := sub(first flatten entries last coefficients f, coefficientRing R);
    (re, im) := (realPart c, imaginaryPart c);
    b := flatten entries ab;
    intervalCCi(re * (product apply(length gensR, i -> (b#i)^(degree(gensR#i, f)))),
	im * (product apply(length gensR, i -> (b#i)^(degree(gensR#i, f)))))
    )
subOnTermCC(RingElement, CCiMatrix) := (f, ab) -> (
    if f == 0 then return intervalCCi 0;
    R := ring f;
    gensR := gens R;
    c := sub(first flatten entries last coefficients f, coefficientRing R);
    (re, im) := (realPart c, imaginaryPart c);
    b := flatten entries ab;
    intervalCCi(re * (product apply(length gensR, i -> (b#i)^(degree(gensR#i, f)))),
	im * (product apply(length gensR, i -> (b#i)^(degree(gensR#i, f)))))
    )



subFloat = method()
subFloat(Number, Matrix) := (f, ab) -> (
    interval(f,f)
    )
subFloat(RingElement, Matrix) := (f, ab) -> (
    if f == 0 then return interval 0;
    listOfTerms := terms f;
    sum apply(listOfTerms, i -> subOnTerm(i, ab))
    )
subFloat(RingElement, CCiMatrix) := (f, ab) -> (
    if f == 0 then return intervalCCi 0;
    listOfTerms := terms f;
    sum apply(listOfTerms, i -> subOnTerm(i, ab))
    )
subFloat(Matrix, Matrix) := (F, ab) -> (
    A := apply(flatten entries F, i -> subFloat(i, ab));
    if class first A === CCi then return matrixCCi {A}
    else return matrix {A}
    )



------ Functions for Krawczyk method
------------------------------------


pointToMatrix = method()
pointToMatrix (Point, Number) := (p, r) -> (
    c := coordinates p;
    if any(c, i -> imaginaryPart i =!= 0) then (
	matrixCCi{apply(c, i ->
		intervalCCi(interval((realPart i) - r, (realPart i) + r),
		    interval((imaginaryPart i) - r, (imaginaryPart i) + r)))}
	)
    else (
    matrix{apply(c, i -> interval(i-r,i+r))}
    )
    )
pointToMatrix (PolySystem, Point) := (F, rp) -> (
    jacOfI := jacobian F;
    evalJac := evaluate(jacOfI, rp);
    Y := inverse evalJac;
    I := matrixCCi rp;
    eqF := equations F;
    u := (2.22*1e-16)^(-1/4); --machine precision
    evalF := transpose matrixCCi {eqF/(i -> subFloat(i, I))};
    radii := (flatten entries(Y*evalF))/(i ->
	u*max{abs(right realPart i), abs(left realPart i)}
	+ ii*u*max{abs(right imaginaryPart i), abs(left imaginaryPart i)});
    coords := coordinates rp;
    matrixEntries := apply(length coords, i ->
	intervalCCi(interval(realPart coords#i - realPart radii#i,realPart coords#i + realPart radii#i)
	    , interval(imaginaryPart coords#i - imaginaryPart radii#i, imaginaryPart coords#i + imaginaryPart radii#i)));
    matrixCCi {matrixEntries}
    )



matrixCCiApply = method()
matrixCCiApply (Matrix,FunctionClosure) := (M,F) -> (matrixCCi (apply(entries M,L->apply(L,i->F i))))
matrixCCiApply (CCiMatrix,FunctionClosure) := (M,F) -> (matrixCCi (apply(M,L->apply(L,i->F i))))


generalizedKrawczykOperator = (y,Y,Fy,DF',X) -> (
    H := id_((ring y)^(numrows y))-Y*DF';
    y - Y*Fy+H*(X-y))

generalizedKrawczykOperatorNormCC = (F,X) -> (
    ym := midpointMatrix X;
	jac := transpose jacobian(F);
	DF' := matrixCCiApply (jac,i->subFloat(i,X));
	midJac := matrix apply(DF',i->apply(i, j -> midpointCCi j));
	Y := midJac^(-1);
	Fy := transpose sub(F,ym);
	ty := transpose ym;
    H := id_((ring ym)^(numrows ty))-Y*DF';
    norm H
    )


midpointMatrix = method()
midpointMatrix (Matrix) := I -> matrixApply(I,i->midpoint i)
midpointMatrix (CCiMatrix) := I -> matrix apply(I,L->apply(L,i->midpointCCi i))



krawczykOperator = method()
krawczykOperator (Ideal,Point) := (F,X) -> krawczykOperator(F, pointToMatrix(F, X))
krawczykOperator (Ideal,CCiMatrix) := (F,X) -> (
    	F = gens F;
    	ym := midpointMatrix X;
    	CR := coefficientRing ring F;
	jac := transpose jacobian(F);
	jacInterval := matrixCCiApply (jac,i->subFloat(i,X));
	midJac := matrix apply(jacInterval,i->apply(i, j -> midpointCCi j));
	Y := midJac^(-1);
	transpose generalizedKrawczykOperator(transpose ym,Y,transpose sub(F,ym),jacInterval,transpose X)
)
krawczykOperator (Ideal,Matrix) := (F,X) -> (
    	F = gens F;
    	CR := coefficientRing ring F;
	if class CR === ComplexField then (
	    X = matrixCCi apply(entries X, i -> apply(i, j -> intervalCCi j));
	    return krawczykOperator(F,X);
	    );
    	ym := midpointMatrix X;
	jac := transpose jacobian(F);
	if precision CR < infinity then (
		jacInterval := matrixApply (jac, i -> subFloat(i, X));
		midJac := matrixApply(jacInterval,i->midpoint i);
		)
	else (
	    	jacInterval = matrixApply (jac, i -> sub(i, X));
		midJac = matrixApply(jacInterval,i->midpoint i);
		);
	Y := midJac^(-1);
	transpose generalizedKrawczykOperator(transpose ym,Y,transpose sub(F,ym),jacInterval,transpose X)
)

krawczykOperator (List,Matrix) := (F,X) -> krawczykOperator(ideal matrix{F},X)
krawczykOperator (List,CCiMatrix) := (F,X) -> krawczykOperator(ideal matrix{F},X)
krawczykOperator (PolySystem,Matrix) := (F,X) -> krawczykOperator(equations F,X)
krawczykOperator (PolySystem,CCiMatrix) := (F,X) -> krawczykOperator(equations F,X)
krawczykOperator (List,Point) := (F,X) -> krawczykOperator(ideal matrix{F}, pointToMatrix(F, X))
krawczykOperator (PolySystem,Point) := (F,X) -> krawczykOperator(equations F, pointToMatrix(F, X))

krawczykTest = method()
krawczykTest (Ideal,Point) := (F,X) -> krawczykTest(F, pointToMatrix(F, X))
krawczykTest (Ideal,CCiMatrix) := (F,X) -> (
    	ym := midpointMatrix X;
    	CR := coefficientRing ring F;
	(n, I) := (generalizedKrawczykOperatorNormCC(gens F,X),krawczykOperator(F,X));
	if {true} == unique apply(flatten entries I,flatten entries X,(i,j)->isSubset(i,j)) then
	   (
		if n < 1/sqrt(2) then true else (
--		    print "Certifying uniqueness fail";
		    false
		    )
	   )
-*	 else (
	     print "Certifying existence fail";
	     false
	     )*-
	 else if all(apply(flatten entries I, flatten entries X, (i,j) ->
	     isEmpty intersect(i,j)), k -> k == false) then (
--	 s := matrixCCi {apply(flatten entries I, flatten entries X, (i, j) ->
--		 intersect(i,j))};
--	 print("No conclusion! Try again with the intersection of the input and Krawczyk operator.");
	 false
	 )
     else (
--	 print("There is no solution in the input");
     	 false
	 )
)
krawczykTest (Ideal,Matrix) := (F,X) ->
(
    	ym := midpointMatrix X;
    	CR := coefficientRing ring F;
	if class CR === ComplexField then (
	    X = matrixCCi apply(entries X, i -> apply(i, j -> intervalCCi j));
	    return krawczykTest(F,X);
	    );
	I := krawczykOperator(F,X);
	if {true} == unique apply(flatten entries I,flatten entries X,(i,j)->isSubset(i,j)) then
	   (
		m := max flatten entries matrixApply(I,i->diameter(i)/2);
		M := max flatten entries matrixApply(X,i->diameter(i)/2);
		if m < M then true else false
	   )
	 else if all(apply(flatten entries I, flatten entries X, (i,j) ->
	     isEmpty intersect(i,j)), k -> k == false) then (
--	 s := matrix {apply(flatten entries I, flatten entries X, (i, j) ->
--		 intersect(i,j))};
--	 print("No conclusion! Try again with the intersection of the input and Krawczyk operator.");
	 false
	 )
     else (
--	 print("There is no solution in the input");
     	 false
	 )
)
krawczykTest (List,Matrix) := (F,X) -> krawczykTest(ideal matrix{F},X)
krawczykTest (List,CCiMatrix) := (F,X) -> krawczykTest(ideal matrix{F},X)
krawczykTest (PolySystem,Matrix) := (F,X) -> krawczykTest(equations F,X)
krawczykTest (PolySystem,CCiMatrix) := (F,X) -> krawczykTest(equations F,X)
krawczykTest (List,Point) := (F,X) -> krawczykTest(ideal matrix{F}, pointToMatrix(F, X))
krawczykTest (PolySystem,Point) := (F,X) -> krawczykTest(equations F, pointToMatrix(F, X))

krawczykRealnessTest = method()
krawczykRealnessTest (Ideal,Point) := (F,X) -> krawczykRealnessTest(F, pointToMatrix(F, X))
krawczykRealnessTest (Ideal, CCiMatrix) := (F,X) -> (
    	ym := midpointMatrix X;
    	CR := coefficientRing ring F;
	(n, I) := (generalizedKrawczykOperatorNormCC(gens F,X),krawczykOperator(F,X));
	if {true} == unique apply(flatten entries I,flatten entries X,(i,j)->isSubset(i,j)) then
	   (
		if n >= 1/sqrt(2) then (
		    false
		    )
		else (
		    entriesI := flatten entries I;
		    conjugateIntervals := matrixCCi {apply(entriesI, i ->
			intervalCCi(realPart i,(-1)*(imaginaryPart i)))};
		    if {true} == unique apply(flatten entries conjugateIntervals,flatten entries X,(i,j)->isSubset(i,j)) then true
		    else false
		    )
		)
	    else false
	    )
krawczykRealnessTest (List,CCiMatrix) := (F,X) -> krawczykRealnessTest(ideal matrix{F},X)
krawczykRealnessTest (PolySystem,CCiMatrix) := (F,X) -> krawczykRealnessTest(equations F,X)
krawczykRealnessTest (List,Point) := (F,X) -> krawczykRealnessTest(ideal matrix{F}, pointToMatrix(F, X))
krawczykRealnessTest (PolySystem,Point) := (F,X) -> krawczykRealnessTest(equations F, pointToMatrix(F, X))



matrixApply = method()
matrixApply (Matrix,FunctionClosure) := (M,F) -> (
    A := apply(entries M,L->apply(L,i->F i));
    if class first A === CCi then return matrixCCi A
    else matrix(A)
    )

-*intervalEval = method(Options => {Strategy=>Substitution})
intervalEval (RingElement,Matrix) := o -> (f,X) -> (
	     if o.Strategy == Substitution then sub(f,X))
*-


--------Multiple roots certification
------------------------------------

certifySingularSolution = method(Options => {
	Strategy => "alphaTheory"})
certifySingularSolution(PolySystem, Point, Number) := opts -> (P, L, iL) -> (
    if opts.Strategy == "alphaTheory" then return certifySingularSolutionAlpha(P,L,iL)
    else if opts.Strategy == "intervalArithmetic" then return certifySingularSolutionInterval(P,L,iL)
    else error "Unknown strategy";
    )
certifySingularSolution(PolySystem, Point) := opts -> (P, L) -> (
    if opts.Strategy == "alphaTheory" then return certifySingularSolutionAlpha(P,L)
    else if opts.Strategy == "intervalArithmetic" then return certifySingularSolutionInterval(P,L)
    else error "Unknown strategy";
    )
certifySingularSolution(PolySystem, CCiMatrix, Number) := opts -> (P, L, iL) -> (
    if opts.Strategy == "alphaTheory" then return error "Try with the Strategy intervalArithmetic"
    else if opts.Strategy == "intervalArithmetic" then return certifySingularSolutionInterval(P,L, iL)
    else error "Unknown strategy";
    )
certifySingularSolution(PolySystem, CCiMatrix) := opts -> (P, L) -> (
    if opts.Strategy == "alphaTheory" then return error "Try with the Strategy intervalArithmetic"
    else if opts.Strategy == "intervalArithmetic" then return certifySingularSolutionInterval(P,L)
    else error "Unknown strategy";
    )
certifySingularSolution(PolySystem, Matrix, Number) := opts -> (P, L, iL) -> (
    if opts.Strategy == "alphaTheory" then return certifySingularSolutionAlpha(P,point L, iL)
    else if opts.Strategy == "intervalArithmetic" then return certifySingularSolutionInterval(P,point L, iL)
    else error "Unknown strategy";
    )
certifySingularSolution(PolySystem, Matrix) := opts -> (P, L) -> (
    if opts.Strategy == "alphaTheory" then return certifySingularSolutionAlpha(P,point L)
    else if opts.Strategy == "intervalArithmetic" then return certifySingularSolutionInterval(P,point L)
    else error "Unknown strategy";
    )
certifySingularSolution(Ideal, Point, Number) := opts -> (P, L, iL) -> certifySingularSolution(polySystem P, L, iL)
certifySingularSolution(Ideal, Point) := opts -> (P, L) -> certifySingularSolution(polySystem P, L)
certifySingularSolution(Ideal, Matrix, Number) := opts -> (P, L, iL) -> certifySingularSolution(polySystem P, point L, iL)
certifySingularSolution(Ideal, Matrix) := opts -> (P, L) -> certifySingularSolution(polySystem P, point L)
certifySingularSolution(Ideal, CCiMatrix, Number) := opts -> (P, L, iL) -> certifySingularSolution(polySystem P, L, iL)
certifySingularSolution(Ideal, CCiMatrix) := opts -> (P, L) -> certifySingularSolution(polySystem P, L)
certifySingularSolution(List, Point, Number) := opts -> (P, L, iL) -> certifySingularSolution(polySystem P, L, iL)
certifySingularSolution(List, Point) := opts -> (P, L) -> certifySingularSolution(polySystem P, L)
certifySingularSolution(List, Matrix, Number) := opts -> (P, L, iL) -> certifySingularSolution(polySystem P, point L, iL)
certifySingularSolution(List, Matrix) := opts -> (P, L) -> certifySingularSolution(polySystem P, point L)
certifySingularSolution(List, CCiMatrix, Number) := opts -> (P, L, iL) -> certifySingularSolution(polySystem P, L, iL)
certifySingularSolution(List, CCiMatrix) := opts -> (P, L) -> certifySingularSolution(polySystem P, L)
certifySingularSolution(PolySystem, List, Number) := opts -> (P, L, iL) -> (
    apply(L, i -> certifySingularSolution(P, i, iL))
    )
certifySingularSolution(Ideal, List, Number) := opts -> (P, L, iL) -> (
    apply(L, i -> certifySingularSolution(polySystem P, i, iL))
    )
certifySingularSolution(List, List, Number) := opts -> (P, L, iL) -> (
    apply(L, i -> certifySingularSolution(polySystem P, i, iL))
    )


certifySingularSolutionInterval = method()
certifySingularSolutionInterval(PolySystem, Point, Number) := (F, P, iter) -> certifySingularSolutionInterval(F, pointToMatrix(F, P), iter)
certifySingularSolutionInterval(PolySystem, Matrix, Number) := (F, P, iter) -> (
    I := ideal F;
    n := numcols gens I;
    R := ring I;
    CR := coefficientRing ring I;
    ym := point matrixApply(P,i->midpoint i);
    dummyF1 := F;
    FList := {F};
    for i from 1 to iter do (
    	jacOfI1 := jacobian dummyF1;
    	nK1 := numericalKernel(evaluate(jacOfI1, ym));
    	k := numcols nK1;
    	u := sub(random(CR^1, CR^k) * transpose nK1, R);
	dummyF1 = polySystem(transpose gens ideal dummyF1 + jacOfI1 * transpose u);
	FList = append(FList,dummyF1);
	);
    F1 := polySystem(transpose(gens ideal F + gens ideal dummyF1));
    krawczykTest(F1, P)
    )
certifySingularSolutionInterval(PolySystem, CCiMatrix, Number) := (F, P, iter) -> (
    I := ideal F;
    n := numcols gens I;
    R := ring I;
    CR := coefficientRing ring I;
    ym := point matrix apply(P,L->apply(L,i->midpointCCi i));
    dummyF1 := F;
    FList := {F};
    for i from 1 to iter do (
    	jacOfI1 := jacobian dummyF1;
    	nK1 := numericalKernel(evaluate(jacOfI1, ym));
    	k := numcols nK1;
    	u := sub(random(CR^1, CR^k) * transpose nK1, R);
	dummyF1 = polySystem(transpose gens ideal dummyF1 + jacOfI1 * transpose u);
	FList = append(FList,dummyF1);
	);
    F1 := polySystem(transpose(gens ideal F + gens ideal dummyF1));
    krawczykTest(F1, P)
    )
certifySingularSolutionInterval(PolySystem, Point) := (F, P) -> certifySingularSolutionInterval(F, pointToMatrix(F, P))
certifySingularSolutionInterval(PolySystem, Matrix) := (F, P) -> (
    y := midpointMatrix P;
    I := ideal F;
    n := numcols gens I;
    R := ring I;
    CR := coefficientRing ring I;
    ym := point matrixApply(P,i->midpoint i);
    dummyF1 := F;
    FList := {F};
    jacOfI1 := jacobian dummyF1;
    evalJac := evaluate(jacOfI1, ym);
    while isFullNumericalRank(evalJac) == false do (
    	nK1 := numericalKernel(evaluate(jacOfI1, ym));
    	k := numcols nK1;
    	u := sub(random(CR^1, CR^k) * transpose nK1, R);
	dummyF1 = polySystem(transpose gens ideal dummyF1 + jacOfI1 * transpose u);
    	jacOfI1 = jacobian dummyF1;
    	evalJac = evaluate(jacOfI1, ym);
	FList = append(FList,dummyF1);
	);
    F1 := polySystem(transpose(gens ideal F + gens ideal dummyF1));
    krawczykTest(F1, P)
    )
certifySingularSolutionInterval(PolySystem, CCiMatrix) := (F, P) -> (
    y := midpointMatrix P;
    I := ideal F;
    n := numcols gens I;
    R := ring I;
    CR := coefficientRing ring I;
    ym := point matrix apply(P,L->apply(L,i->midpointCCi i));
    dummyF1 := F;
    FList := {F};
    jacOfI1 := jacobian dummyF1;
    evalJac := evaluate(jacOfI1, ym);
    while isFullNumericalRank(evalJac) == false do (
    	nK1 := numericalKernel(evalJac);
    	k := numcols nK1;
    	u := sub(random(CR^1, CR^k) * transpose nK1, R);
	dummyF1 = polySystem(transpose gens ideal dummyF1 + jacOfI1 * transpose u);
    	jacOfI1 = jacobian dummyF1;
    	evalJac = evaluate(jacOfI1, ym);
	FList = append(FList,dummyF1);
	);
    F1 := polySystem(transpose(gens ideal F + gens ideal dummyF1));
    krawczykTest(F1, P)
    )

certifySingularSolutionAlpha = method()
certifySingularSolutionAlpha(PolySystem, Point, Number) := (F, P, iter) -> (
    I := ideal F;
    n := numcols gens I;
    R := ring I;
    CR := coefficientRing ring I;
    dummyF1 := F;
    FList := {F};
    for i from 1 to iter do (
    	jacOfI1 := jacobian dummyF1;
    	nK1 := numericalKernel(evaluate(jacOfI1, P));
    	k := numcols nK1;
	--    v := sub(random(CR^1, CR^k) * transpose nK, R);
    	u := sub(random(CR^1, CR^k) * transpose nK1, R);
	dummyF1 = polySystem(transpose gens ideal dummyF1 + jacOfI1 * transpose u);
	FList = append(FList,dummyF1);
	);
    F1 := polySystem(transpose(gens ideal F + gens ideal dummyF1));
    certifyRegularSolution(F1, P)
    )
certifySingularSolutionAlpha(PolySystem, Point) := (F, P) -> (
    I := ideal F;
    n := numcols gens I;
    R := ring I;
    CR := coefficientRing ring I;
    dummyF1 := F;
    FList := {F};
    jacOfI1 := jacobian dummyF1;
    evalJac := evaluate(jacOfI1, P);
    while isFullNumericalRank(evalJac) == false do (
    	nK1 := numericalKernel(evalJac);
    	k := numcols nK1;
    	u := sub(random(CR^1, CR^k) * transpose nK1, R);
	dummyF1 = polySystem(transpose gens ideal dummyF1 + jacOfI1 * transpose u);
    	jacOfI1 = jacobian dummyF1;
    	evalJac = evaluate(jacOfI1, P);
	FList = append(FList,dummyF1);
	);
    F1 := polySystem(transpose(gens ideal F + gens ideal dummyF1));
    certifyRegularSolution(F1, P)
    )




TEST ///
R = RR[x1,x2,y1,y2]
f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -3.5,x1^2 + y1^2 -1, x2^2 + y2^2 - 1}
I1 = interval(.90,.96)
I2 = interval(.31,.33)
I3 = interval(-.33,-.27)
I4 = interval(.9,1)
krawczykTest(f,matrix{{I1,I2,I3,I4}})
///




beginDocumentation()
load ("./NumericalCertification/Documents/DocNumericalCertification.m2")
end





restart
check "NumericalCertification"
uninstallAllPackages()
installPackage "NumericalCertification"
debug needsPackage("NumericalCertification", Configuration => {"ALPHACERTIFIEDexec" => "~/Dropbox/Math/alphaCertifiedCode/"})
needsPackage("NumericalCertification", Configuration => {"ALPHACERTIFIEDexec" => "~/Dropbox/Math/alphaCertifiedCode/"})
viewHelp NumericalCertification


restart
installPackage "NumericalCertification"
viewHelp NumericalCertification
