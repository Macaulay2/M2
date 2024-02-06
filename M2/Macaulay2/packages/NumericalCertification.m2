newPackage(
	"NumericalCertification",
    	Version => "1.6",
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
    "pointToInterval",
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
-*pointNorm(AbstractPoint) := x -> (
    pointNorm matrix x
    )*-
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


computeConstants = method() --computes alpha^2 beta^2 gamma^2
computeConstants(PolySystem, Matrix) := (f, x) -> (
    computeConstants(f, point x)
    )
computeConstants(PolySystem, AbstractPoint) := (f, x) -> (
    slpF := gateSystem f;
    eqs := equations f;
    n := length eqs;
    R := coefficientRing ring f;
    J := transpose reshape(R^n,R^n,evaluate(jacobian slpF, matrix x));
    inverseJ := inverse J;
    pointNormx := pointNorm matrix x;
    degs := select(flatten apply(eqs, i -> degree i), i -> i =!= 0);
    if precision R =!= infinity then (
	R = R;
    	if det J == 0 then error "The Jacobian is not invertible";
    	-- beta
    	y := point(inverseJ * transpose evaluate(slpF,matrix x));
    	beta := sub(sum apply(y#Coordinates, i -> abs(i)^2),R);
    	deltaD := diagonalMatrix flatten apply(degs, i -> sqrt(i * (pointNormx)^(i-1))); 
     	mu := max {1, polySysNorm(f) * (norm(2,inverseJ * deltaD))^2};
	)
    else if R =!= QQ then (
	var := first gens R;
	R = coefficientRing R;
    	if det J == 0 then error "The Jacobian is not invertible";
    	-- beta
    	y = point(inverseJ * transpose evaluate(slpF, matrix x));
    	beta = sub(sum apply(y#Coordinates, i -> sqabsForGaussianRational i),R);
    	deltaD = diagonalMatrix flatten apply(degs, i -> i * (pointNormx)^(i-1)); 
	sqFrobenius := trace(inverseJ * deltaD * (transpose conjugateGaussianRationalMatrix inverseJ));
     	mu = max {1, polySysNorm(f) * (sub((coefficient(var, sqFrobenius))^2, R) + sub((sqFrobenius - var * coefficient(var,sqFrobenius)), R))};  
	)
    else (
	R = R;
    	if det J == 0 then error "The Jacobian is not invertible";
    	-- beta
    	y = point(inverseJ * transpose evaluate(slpF,matrix x));
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
certifyRegularSolution(PolySystem, AbstractPoint) := (f, x) -> (
    alpha := first computeConstants(f,x);
    -- check: alpha < (13-3*sqrt(17))/4
    if 16*alpha < 169 and (322-16*alpha)^2 > 78*78*17 then true else false
    )
certifyRegularSolution(PolySystem, AbstractPoint, Sequence) := (f, x, consts) -> (
    alpha := consts#0;
    if 16*alpha < 169 and (322-16*alpha)^2 > 78*78*17 then true else false
    )
certifyRegularSolution(PolySystem, Matrix, Sequence) := (f, x, consts) -> (
    certifyRegularSolution(f, point x, consts)
    ) 

certifyDistinctSolutions = method()
certifyDistinctSolutions(PolySystem, Matrix, Matrix) := (f, x1, x2) -> (
    certifyDistinctSolutions(f, point x1, point x2)
    )
certifyDistinctSolutions(PolySystem, AbstractPoint, AbstractPoint) := (f, x1, x2) -> (
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
    consts1 := computeConstants(f,x1);
    consts2 := computeConstants(f,x2);
    if precision R =!= infinity then (
    	normOfDist := (norm(2,point{(coordinates x1)-(coordinates x2)}))^2;
	)
    else (
    	normOfDist = sum apply((point{(coordinates x1)-(coordinates x2)})#Coordinates, c->sub(c^2,R));
	);
    if consts1 #0 >= ((13-3*sqrt(17))/4)^2 or consts2 #0 >= ((13-3*sqrt(17))/4)^2 then (
	false
	)
    else if normOfDist > 4*((consts1)#1 + (consts2)#1 + 2*sqrt((consts1)#1 * (consts2)#1)) then (
	true
	)
    else if (consts1)#0 < 9/10000 and normOfDist < 1/(400*(consts1)#2) or (consts2)#0 < 9/10000 and normOfDist < 1/(400*(consts2)#2) then (
	false
	)
    else (
      	false
	)
    )
certifyDistinctSolutions(PolySystem, List, List) := (f, x1, x2) -> (
    consts1 := x1#1;
    consts2 := x2#1;
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
    if precision R =!= infinity then (
    	normOfDist := (norm(2,point{(coordinates x1#0)-(coordinates x2#0)}))^2;
	)
    else (
    	normOfDist = sum apply((point{(coordinates x1#0)-(coordinates x2#0)})#Coordinates, c->sub(c^2,R));
	);
    if consts1#0 >= ((13-3*sqrt(17))/4)^2 or consts2#0 >= ((13-3*sqrt(17))/4)^2 then (
	false
	)
    else if normOfDist > 4*((consts1)#1 + (consts2)#1 + 2*sqrt((consts1)#1 * (consts2)#1)) then (
	true
	)
    else if (consts1)#0 < 9/10000 and normOfDist < 1/(400*(consts1)#2) or (consts2)#0 < 9/10000 and normOfDist < 1/(400*(consts2)#2) then (
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
certifyRealSolution(PolySystem, AbstractPoint) := (f, x) -> (
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
    false
    )
    )
certifyRealSolution(PolySystem, Matrix,Sequence) := (f, x, consts) -> (
    certifyRealSolution(f, point x, consts)
    )
certifyRealSolution(PolySystem, AbstractPoint,Sequence) := (f, x, consts) -> (
    (alpha,beta,gamma) := consts;
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
    	false
    	)
    )

alphaTheoryCertification = method()
alphaTheoryCertification(PolySystem, List) := (f, X) -> (
    if class first X =!= List then (
	    X = apply(X, i -> {i, computeConstants(f,i)});
    	    );
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
    	Y := select(X, i->certifyRegularSolution(f,i#0,i#1)=!=false); 
    	C := apply(X, i-> i#1#0); 
    	S := new MutableList from Y;
    	for i from 0 to length(Y) - 1 do S#i = true;
    	for i from 0 to length(Y) - 2 do for j from i+1 to length(Y) - 1 do if (
	    S#i == true and S#j == true
	    )
    	then (
	    S#j = certifyDistinctSolutions(f,Y#i, Y#j);
	    );
    	D := delete( ,apply(length(Y), i -> if S#i == true then Y#i else null));--{};
    	if R =!= CC then (
	    Real := delete( ,apply(length(D), i -> 
	    if certifyRealSolution(f,D#i#0,D#i#1) == true then D#i
	    else null));
    	    );
	Y = apply(Y, i -> i#0);
	D = apply(D, i -> i#0);
	Real = apply(Real, i -> i#0);
    	new MutableHashTable from {"certifiedRegular" => Y, "alphaValues" => C, "certifiedDistinct" =>D, "certifiedReal" => Real}
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
-- input : Matrix representing a coordinate of a point or AbstractPoint
-- output : a list of coordinates of a given point.
pointBlock = method()
pointBlock(AbstractPoint) := P -> (
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


certifySolutionsAlpha = method()
certifySolutionsAlpha(PolySystem, List, Number) := (P, L, iL) -> (
    X := apply(L, i -> {i, computeConstants(P, i)});
    regSolutions := delete( ,apply(X, i -> if certifyRegularSolution(P, i#0, i#1) then i else null));
    apply(regSolutions, i -> L = delete(first i, L));
    multSolutions := delete( ,apply(L, i -> (
	    iter := 1;
	    c := certifySingularSolutionAlpha(P, i, iter);
	    while c == false and iter <= iL do (
		iter = iter + 1;
	    	c = certifySingularSolutionAlpha(P, i, iter);
		);
	    if c == true then i else null
	    )));
    apply(multSolutions, i -> L = delete(i, L));
    if length regSolutions == 0 then (
    	new MutableHashTable from {"certifiedRegular" => {},
	    "alphaValues" => {},
	    "certifiedDistinct" => {},
	    "certifiedReal" => {},
	    "certifiedSingular" => multSolutions,
	    "nonCertified" => L}
	)
    else (ac := alphaTheoryCertification(P, regSolutions);
    	new MutableHashTable from {"certifiedRegular" => ac#"certifiedSolutions",
	    "alphaValues" => ac#"alphaValues",
	    "certifiedDistinct" => ac#"certifiedDistinct",
	    "certifiedReal" => ac#"certifiedReal",
	    "certifiedSingular" => multSolutions,
	    "nonCertified" => L}
	)
    )
certifySolutionsAlpha(PolySystem, List) := (P, L) -> (
    X := apply(L, i -> {i, computeConstants(P, i)});
    regSolutions := delete( ,apply(X, i -> if certifyRegularSolution(P, i#0, i#1) then i else null));
    apply(regSolutions, i -> L = delete(first i, L));
    multSolutions := delete( ,apply(L, i -> (
	    iter := 1;
	    c := certifySingularSolutionAlpha(P, i);
	    if c == true then i else null
	    )));
    apply(multSolutions, i -> L = delete(i, L));
    if length regSolutions == 0 then (
    	new MutableHashTable from {"certifiedRegular" => {},
	    "alphaValues" => {},
	    "certifiedDistinct" => {},
	    "certifiedReal" => {},
	    "certifiedSingular" => multSolutions,
	    "nonCertified" => L}
	)
    else (ac := alphaTheoryCertification(P, regSolutions);
    	new MutableHashTable from {"certifiedRegular" => ac#"certifiedRegular",
	    "alphaValues" => ac#"alphaValues",
	    "certifiedDistinct" => ac#"certifiedDistinct",
	    "certifiedReal" => ac#"certifiedReal",
	    "certifiedSingular" => multSolutions,
	    "nonCertified" => L}
	)
    )


certifySolutionsInterval = method()
certifySolutionsInterval(PolySystem, List, Number) := (P, I, iL) -> (
    I = apply(I, i -> {pointToInterval(P,i),krawczykOperator(P, i)});
    regSolutions := delete( ,apply(#I, i -> if krawczykTest(P, I#i) then I#i));
    apply(regSolutions, i -> I = delete(i, I));
    multSolutions := delete( ,apply(#I, i -> (
	    iter := 1;
	    c := certifySingularSolutionInterval(P, I#i#0, iter);
	    while c == false and iter <= iL do (
		iter = iter + 1;
	    	c = certifySingularSolutionInterval(P, I#i#0, iter);
		);
	    if c == true then I#i else null
	    )));
    apply(multSolutions, i -> I = delete(i, I));
    new MutableHashTable from {"certifiedRegular" => apply(regSolutions, i -> 
	    i#1#0),
	 "certifiedReal" => delete( ,apply(regSolutions, i -> 
	     if krawczykRealnessTest(P, i) then i#1#0 else null)),
	 "certifiedSingular" => apply(multSolutions, i -> i#1#0),
	 "nonCertified" => apply(I, i -> i#0)}
    )
certifySolutionsInterval(PolySystem, List) := (P, I) -> (
    I = apply(I, i -> {pointToInterval(P,i),krawczykOperatorAndNorm(P, i)});
    regSolutions := delete( ,apply(#I, i -> if krawczykTest(P, I#i) then I#i));
    apply(regSolutions, i -> I = delete(i, I));
    multSolutions := delete( ,apply(#I, i -> (
	    iter := 1;
	    c := certifySingularSolutionInterval(P, I#i#0, iter);
	    if c == true then I#i else null
	    )));
    apply(multSolutions, i -> I = delete(i, I));
    new MutableHashTable from {"certifiedRegular" => apply(regSolutions, i -> 
	    i#1#0),
	 "certifiedReal" => delete( ,apply(regSolutions, i -> 
	     if krawczykRealnessTest(P, i) then i#1#0 else null)),
	 "certifiedSingular" => apply(multSolutions, i -> i#1#0),
	 "nonCertified" => apply(I, i -> i#0)}
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
matrixCCi AbstractPoint := i -> new CCi from (
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


pointToInterval = method()
pointToInterval (AbstractPoint, Number) := (p, r) -> (
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
pointToInterval (PolySystem, AbstractPoint) := (F, rp) -> (
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

generalizedKrawczykOperatorNormCC = (y,Y,Fy,DF',X) -> (
    H := id_((ring y)^(numrows y))-Y*DF';
    norm H
    )


midpointMatrix = method()
midpointMatrix (Matrix) := I -> matrixApply(I,i->midpoint i)
midpointMatrix (CCiMatrix) := I -> matrix apply(I,L->apply(L,i->midpointCCi i))



krawczykOperatorAndNorm = method()
krawczykOperatorAndNorm (Matrix,AbstractPoint) := (F,X) -> krawczykOperatorAndNorm(F, pointToInterval(F, X))
krawczykOperatorAndNorm (Matrix,CCiMatrix) := (F,X) -> (
    	ym := midpointMatrix X;
    	CR := coefficientRing ring F;
	jac := transpose jacobian(F);
	jacInterval := matrixCCiApply (jac,i->subFloat(i,X));
	midJac := matrix apply(jacInterval,i->apply(i, j -> midpointCCi j));
	Y := midJac^(-1);
	(transpose generalizedKrawczykOperator(transpose ym,Y,transpose sub(F,ym),jacInterval,transpose X),
	    generalizedKrawczykOperatorNormCC(transpose ym,Y,transpose sub(F,ym),jacInterval,transpose X))
)
krawczykOperatorAndNorm (PolySystem,CCiMatrix) := (F,X) -> krawczykOperatorAndNorm(matrix{equations F},X)
krawczykOperatorAndNorm (PolySystem,AbstractPoint) := (F,X) -> krawczykOperatorAndNorm(matrix{equations F}, pointToInterval(F, X))

krawczykOperator = method()
krawczykOperator (Matrix,AbstractPoint) := (F,X) -> krawczykOperatorAndNorm(F, pointToInterval(F, X))
krawczykOperator (Matrix,CCiMatrix) := (F,X) -> (
    	ym := midpointMatrix X;
    	CR := coefficientRing ring F;
	jac := transpose jacobian(F);
	jacInterval := matrixCCiApply (jac,i->subFloat(i,X));
	midJac := matrix apply(jacInterval,i->apply(i, j -> midpointCCi j));
	Y := midJac^(-1);
	transpose generalizedKrawczykOperator(transpose ym,Y,transpose sub(F,ym),jacInterval,transpose X)
)
krawczykOperator (Matrix,Matrix) := (F,X) -> (
    	CR := coefficientRing ring F;
	n := numcols F;
	if class CR === ComplexField then (
	    X = matrixCCi apply(entries X, i -> apply(i, j -> intervalCCi j));
	    return krawczykOperatorAndNorm(F,X);
	    );
    	ym := midpointMatrix X;
	jac := transpose jacobian(F);
	if precision CR < infinity then (
	    jacInterval := matrixApply (jac, i -> subFloat(i, X));
	    midJac := matrixApply(jacInterval,i->midpoint i);
	    )
	else (
	    slpf := gateSystem transpose F;
    	    jacInterval = transpose reshape(RRi^n,RRi^n,evaluate(jacobian slpf, X));
--	    jacInterval = matrixApply (jac, i -> sub(i, X));
	    midJac = matrixApply(jacInterval,i->midpoint i);
	    F = slpf;
	    );
	Y := midJac^(-1);
	transpose generalizedKrawczykOperator(transpose ym,Y,transpose evaluate(F,ym),jacInterval,transpose X)
)


krawczykOperator (PolySystem,Matrix) := (F,X) -> krawczykOperator(matrix{equations F},X)
krawczykOperator (PolySystem,CCiMatrix) := (F,X) -> krawczykOperator(matrix{equations F},X)
krawczykOperator (PolySystem,AbstractPoint) := (F,X) -> krawczykOperator(matrix{equations F}, pointToInterval(F, X))

krawczykTest = method()
krawczykTest (Matrix,AbstractPoint) := (F,X) -> krawczykTest(F, pointToInterval(F, X))
krawczykTest (Matrix,CCiMatrix) := (F,X) -> (
    	ym := midpointMatrix X;
    	CR := coefficientRing ring F;
	(I, n) := krawczykOperatorAndNorm(F,X);
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
krawczykTest (Matrix,List) := (F,X) -> (
	(I, n) := X#1;
	if {true} == unique apply(flatten entries I,flatten entries X#0,(i,j)->isSubset(i,j)) then
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
	 else if all(apply(flatten entries I, flatten entries X#0, (i,j) -> 
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
krawczykTest (Matrix,Matrix) := (F,X) ->
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
krawczykTest (PolySystem,List) := (F,X) -> krawczykTest(matrix{equations F},X)
krawczykTest (PolySystem,Matrix) := (F,X) -> krawczykTest(matrix{equations F},X)
krawczykTest (PolySystem,CCiMatrix) := (F,X) -> krawczykTest(matrix{equations F},X)
krawczykTest (PolySystem,AbstractPoint) := (F,X) -> krawczykTest(matrix{equations F}, pointToInterval(F, X))

krawczykRealnessTest = method()
krawczykRealnessTest (Matrix,AbstractPoint) := (F,X) -> krawczykRealnessTest(F, pointToInterval(F, X))
krawczykRealnessTest (Matrix, CCiMatrix) := (F,X) -> (
    	ym := midpointMatrix X;
    	CR := coefficientRing ring F;
	(I, n) := krawczykOperatorAndNorm(F,X);
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
krawczykRealnessTest (Matrix, List) := (F,X) -> (
	(I, n) := X#1;
	if {true} == unique apply(flatten entries I,flatten entries X#0,(i,j)->isSubset(i,j)) then
	   (
		if n >= 1/sqrt(2) then (
		    false
		    )
		else (
		    entriesI := flatten entries I;
		    conjugateIntervals := matrixCCi {apply(entriesI, i -> 
			intervalCCi(realPart i,(-1)*(imaginaryPart i)))};
		    if {true} == unique apply(flatten entries conjugateIntervals,flatten entries X#0,(i,j)->isSubset(i,j)) then true
		    else false
		    )
		)
	    else false
	    )
krawczykRealnessTest (PolySystem,List) := (F,X) -> krawczykRealnessTest(matrix{equations F},X)
krawczykRealnessTest (PolySystem,CCiMatrix) := (F,X) -> krawczykRealnessTest(matrix{equations F},X)
krawczykRealnessTest (PolySystem,AbstractPoint) := (F,X) -> krawczykRealnessTest(matrix{equations F}, pointToInterval(F, X))



matrixApply = method()
matrixApply (Matrix,FunctionClosure) := (M,F) -> (
    A := apply(entries M,L->apply(L,i->F i));
    if class first A === CCi then return matrixCCi A
    else matrix(A)
    )

intervalEval = method(Options => {Strategy=>"Substitution"})
intervalEval (RingElement,Matrix) := o -> (f,X) -> (
	     if o.Strategy == "Substitution" then sub(f,X))



--------Multiple roots certification
------------------------------------

certifySingularSolution = method(Options => {
	Strategy => "alphaTheory"})
certifySingularSolution(PolySystem, AbstractPoint, Number) := opts -> (P, L, iL) -> (
    if opts.Strategy == "alphaTheory" then return certifySingularSolutionAlpha(P,L,iL)
    else if opts.Strategy == "intervalArithmetic" then return certifySingularSolutionInterval(P,L,iL)
    else error "Unknown strategy";
    )
certifySingularSolution(PolySystem, AbstractPoint) := opts -> (P, L) -> (
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

certifySingularSolutionInterval = method()
certifySingularSolutionInterval(PolySystem, AbstractPoint, Number) := (F, P, iter) -> (
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
    	u := sub(random(CR^1, CR^k) * transpose nK1, R);
	dummyF1 = polySystem(transpose gens ideal dummyF1 + jacOfI1 * transpose u);
	FList = append(FList,dummyF1);
	);
    F1 := polySystem(transpose(gens ideal F + gens ideal dummyF1));
    krawczykTest(F1, pointToInterval(F1,P))
    )
certifySingularSolutionInterval(PolySystem, Matrix, Number) := (F, P, iter) -> (
    y := midpointMatrix P;
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
    y := midpointMatrix P;
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
certifySingularSolutionInterval(PolySystem, AbstractPoint) := (F, P) -> (
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
    krawczykTest(F1, pointToInterval(F1,P))
    )
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
certifySingularSolutionAlpha(PolySystem, AbstractPoint, Number) := (F, P, iter) -> (
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
certifySingularSolutionAlpha(PolySystem, AbstractPoint) := (F, P) -> (
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
certifySingularSolutionAlpha (PolySystem,Matrix,Number) := (F,P,iter) -> certifySingularSolutionAlpha(F, point P,iter)
certifySingularSolutionAlpha (PolySystem,Matrix) := (F,P) -> certifySingularSolutionAlpha(F, point P)








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
