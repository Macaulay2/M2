
newPackage(
        "RandomPoints",
    	Version => "1.5.2",
    	Date => "July 30th, 2021",
    	Authors => {
	     {Name => "Sankhaneel Bisui", Email => "sbisu@tulane.edu", HomePage=>"https://sites.google.com/view/sankhaneelbisui/home"},
         {Name => "Zhan Jiang", Email => "zoeng@umich.edu", HomePage => "http://www-personal.umich.edu/~zoeng/"},
         {Name => "Sarasij Maitra", Email => "sm3vg@virginia.edu", HomePage => "https://sarasij93.github.io/"},         
	     {Name=> "Thai Nguyen", Email =>"tnguyen11@tulane.edu", HomePage=>"https://sites.google.com/view/thainguyenmath "},
         {Name=> "Frank-Olaf Schreyer", Email =>"schreyer@math.uni-sb.de", HomePage=>"https://www.math.uni-sb.de/ag/schreyer/index.php/ "},
	     {Name=>"Karl Schwede", Email=>"schwede@math.utah.edu", HomePage=>"https://www.math.utah.edu/~schwede/" }	     	     
	     },
    	Headline => "find a point in a given variety over a finite field",
        PackageImports => {"SwitchingFields", "MinimalPrimes", "ConwayPolynomials"}, 
		DebuggingMode => false, 
		Reload=>false,
		AuxiliaryFiles => false -- set to true if package comes with auxiliary files
    	)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {
	"genericProjection", --documented, tested
	"projectionToHypersurface", --documented, tested    
	"randomCoordinateChange", --documented, tested
	"randomPoints", 
    --"linearIntersectionNew",
    --"randomPointViaMultiplicationTableNew",
	"extendIdealByNonZeroMinor",
	"findANonZeroMinor",
    --"verifyPoint",    
    --"randomPointViaLinearIntersection", --these are here for debugging purposes
    --"randomPointViaLinearIntersectionOld", --these are here for debugging purposes
    "getRandomLinearForms", --here for debugging purposes    
    "dimViaBezout",        
    --"dimViaBezoutInternal", 
	"Codimension",
	"MaxCoordinatesToReplace",    
    "Replacement",
    "Full", 
    "Trinomial",
    --"Default", --a valid value for [randomPoints, Strategy]
	"BruteForce", --a valid value for [randomPoints, Strategy], documented,     
    "LinearIntersection",  --a valid value for [randomPoints, Strategy]
    "MultiplicationTable", --a valid value for [randomPoints,DecompositionStrategy]	        
    "ExtendField", --an option controls whether the field is extended
    "DimensionFunction", --
    "PointCheckAttempts",
    "MinorPointAttempts",
    "MinimumFieldSize",
    "DecompositionStrategy",
    "DimensionIntersectionAttempts",
    "NumThreadsToUse" -- used in the BruteForce strategy
    }
exportMutable {}

--installMinprimes();

--this appears to need to be here, otherwise the options don't realize dimViaBezout is a function, it thinks its a symbol.
dimViaBezout=method(Options => {
    Verbose => false, 
    Homogeneous => true, 
    DimensionIntersectionAttempts => null, 
    MinimumFieldSize => null,
    Replacement => Full});

optRandomPoints := {
    Strategy=>Default, 
    Homogeneous => true,  
    Replacement => Binomial,    
    ExtendField => false,
    PointCheckAttempts => 0,
    DecompositionStrategy => null,
    NumThreadsToUse => 1,
    DimensionFunction => dim,
    Verbose => false
};

optFindANonZeroMinor := optRandomPoints | {MinorPointAttempts => 5} | {ExtendField => true} | {Homogeneous => false}

optCoorindateChange := {
    Verbose => false, 
    Homogeneous=>true, 
    Replacement=>Full, 
    MaxCoordinatesToReplace => infinity
};

optProjectionToHypersurface := {
    Codimension => null,
    Verbose => false, 
    Homogeneous=>true, 
    Replacement=>Binomial, 
    MaxCoordinatesToReplace => infinity
};

pointToIdeal = method(Options =>{Homogeneous => false});

pointToIdeal(Ring, List) := opts -> (R1, L1) -> (
        if (opts.Homogeneous == false) then (
        genList := gens R1;
        return ideal( apply(#genList, i->genList#i - (sub(L1#i, R1)) ));
        );
);

idealToPoint = method(Options => {Homogeneous => false});

idealToPoint(Ideal) := opts -> (I1) -> (
    if (opts.Homogeneous == false) then (
        genList := gens ring I1;
        return apply(genList, s -> s%I1);
    )
);

--this function was taken directly from an internal function in RationalPoints.m2 by Nathaniel Stapleton
fieldElements = (k) -> (
     J := ideal k;
     p := char k;
     els := {};
     galoisfield := class k === GaloisField;
     if galoisfield then (
          x := k.PrimitiveElement; --sometimes k_0 is not the primitive element ie. GF 9
          e := 1;
          b := 0;
          els = els|{0};
          while b != 1 do (
               b = x^e;
               e = e+1;
               els = els | {b};
               );
          );
     if not galoisfield and char ring J != 0 then (
     	  d := (degree((flatten entries gens J)_0))_0;
     	  a := (gens k)_0;
          coeffs := toList ((set toList (0..p-1)) ^** (d));
     	  for i to # coeffs - 1 do (
               x := 0;
               for j to d-1 do (
               	    x = x+coeffs_i_j*a^j;
               	    );
               els = els | {x};
               );
          );
     if not galoisfield and char ring J == 0 then els = toList (0..p-1);
     return els;
     );



  --Function to create a random point
createRandomPoints= method(TypicalValue => List, Options => {})
createRandomPoints(Ideal):=List => opts->(I1) ->(
    noVar := #generators ring I1;
    K:=coefficientRing ring (I1);
    L:=toList apply(noVar, i ->random(K));
    return L )


randomCoordinateChange = method(Options=>optCoorindateChange);

    --constForms := L1#0;
    --monomialForms := L1#1;
    --trueMonomialForms := L1#2; --forced to be monomial whether or not Homogeneous is true
    --binomialForms := L1#3; 
    --trinomialForms := L1#4;
    --randForms := L1#5;
randomCoordinateChange(Ring) := opts -> (R1) -> (
    if (debugLevel > 0) or (opts.Verbose) then print "randomCoordinateChange: starting.";
    local phi;
    if not class R1 === PolynomialRing then error "randomCoordinateChange: expected a polynomial ring";
    myMon := monoid R1;
    S1 := (coefficientRing R1)(myMon);
    d1 := #gens R1;
    local genList;
    if (opts.Replacement == Binomial) then (
        genList = getRandomLinearForms(R1, {0, max(d1 - opts.MaxCoordinatesToReplace, 0), 0, min(d1, opts.MaxCoordinatesToReplace),0, 0}, Homogeneous => opts.Homogeneous, Verbose=>opts.Verbose, Verify=>true); )
    else if (opts.Replacement == Trinomial) then (
        genList = getRandomLinearForms(R1, {0, max(d1 - opts.MaxCoordinatesToReplace, 0), 0, 0, min(d1, opts.MaxCoordinatesToReplace),0}, Homogeneous => opts.Homogeneous, Verbose=>opts.Verbose, Verify=>true); )
    else if (opts.Replacement == Monomial) then (
        genList = getRandomLinearForms(R1, {0,d1, 0, 0,0, 0}, Homogeneous => opts.Homogeneous, Verbose=>opts.Verbose, Verify=>true); )
    else if (opts.Replacement == Full) then (
        genList = getRandomLinearForms(R1, {0, max(d1 - opts.MaxCoordinatesToReplace, 0), 0, 0, 0, min(d1, opts.MaxCoordinatesToReplace)}, Homogeneous => opts.Homogeneous, Verbose=>opts.Verbose, Verify=>true); );
--    genList = random apply(genCount, t -> if (t < opts.MaxCoordinatesToReplace) then replacementFunction(genList#t) else genList#t);
    return map(R1, S1, genList);
);


genericProjection = method(Options=>optCoorindateChange);

genericProjection(Ideal) := opts -> (I1) -> (
    return genericProjectionByKernel(1, I1, opts);
);

genericProjection(ZZ, Ideal) := opts -> (n1, I1) -> (
    return genericProjectionByKernel(n1, I1, opts);
);

--this code is based upon randomKRationalPoint
genericProjectionOriginal = method(Options=>optCoorindateChange);

genericProjectionOriginal(ZZ, Ideal) := opts -> (n1, I1) -> (
        if (debugLevel > 0) or (opts.Verbose) then print concatenate("genericProjection (dropping ", toString(n1), " dimension):  Starting, Replacement =>", toString(opts.Replacement), ", MaxCoordinatesToReplace => ", toString(opts.MaxCoordinatesToReplace));
        R1 := ring I1;
        psi := randomCoordinateChange(R1, opts);
        flag := true;
        local psiInv;
        while (flag) do (
            try psiInv = inverse(psi) then (flag = false) else (psi = randomCoordinateChange(R1, opts));
        );
        S1 := source psi;
        I2 := psiInv(I1);
        if (n1 <= 0) then return(psi, I2); --if we don't actually want to project
        kk:=coefficientRing R1;
        local Re;
        local Rs;
        Re=kk(monoid[apply(dim S1,i->S1_i), MonomialOrder => Eliminate n1]);
        rs:=first (entries selectInSubring(1,vars Re));
        Rs=kk(monoid[rs]);
        f:=ideal substitute(selectInSubring(1, generators gb substitute(I2,Re)),Rs);
        phi := map(S1, Rs);
        return(psi*phi, f);
);

--using the SubringLimit option, as in Cremona.
genericProjectionByKernel = method(Options=>optCoorindateChange);

genericProjectionByKernel(ZZ, Ideal) := opts -> (n1, I1) -> (
    if (debugLevel > 0) or (opts.Verbose) then print concatenate("genericProjectionByKernel (dropping ", toString(n1), " dimension):  Starting, Replacement =>", toString(opts.Replacement), ", MaxCoordinatesToReplace => ", toString(opts.MaxCoordinatesToReplace));
    R1 := ring I1;
    local psi;
    if not class R1 === PolynomialRing then error "genericProjectionByKernel: expected an ideal in a polynomial ring";
    if (n1 <= 0) then( --if we don't want to project
        return(map(R1, R1), I1); --if we don't actually want to project
    ); 
    kk:=coefficientRing R1;
    myVars := drop(gens R1, n1);
    Rs := kk(monoid[myVars]);
    d2 := #myVars;
    local genList;
    if (opts.Replacement == Binomial) then (
        genList = getRandomLinearForms(R1, {0, max(d2 - opts.MaxCoordinatesToReplace, 0), 0, min(d2, opts.MaxCoordinatesToReplace), 0, 0}, Homogeneous => opts.Homogeneous, Verbose=>opts.Verbose, Verify=>true); )
    else if (opts.Replacement == Trinomial) then (
        genList = getRandomLinearForms(R1, {0, max(d2 - opts.MaxCoordinatesToReplace, 0), 0, 0, min(d2, opts.MaxCoordinatesToReplace), 0}, Homogeneous => opts.Homogeneous, Verbose=>opts.Verbose, Verify=>true); )
    else if (opts.Replacement == Monomial) then (
        genList = getRandomLinearForms(R1, {0, d2, 0, 0, 0, 0}, Homogeneous => opts.Homogeneous, Verbose=>opts.Verbose, Verify=>true); )
    else if (opts.Replacement == Full) then (
        genList = getRandomLinearForms(R1, {0, max(d2 - opts.MaxCoordinatesToReplace, 0), 0, 0, 0, min(d2, opts.MaxCoordinatesToReplace)}, Homogeneous => opts.Homogeneous, Verbose=>opts.Verbose, Verify=>true); )
    else (
        error "genericProjectionByKernel:  not a valid replacement strategy"
    );
--    genList = random apply(genCount, t -> if (t < opts.MaxCoordinatesToReplace) then replacementFunction(genList#t) else genList#t);
    psi = map(R1, Rs, genList);
    myMap := map(R1/I1, Rs, genList);
    K2 := ker(myMap);
    return(psi, K2);
);

genericProjection(ZZ, Ring) := opts -> (n1, R1) -> (
    J1 := ideal R1;
    l1 := genericProjection(n1, J1, opts);
    if (class R1 === PolynomialRing) then (
        return (l1#0, source (l1#0));
    )
    else (
        J2 := l1#1;
        S1 := source(l1#0);
        newMap := map(R1, S1/J2, matrix(l1#0));
        return (newMap, source newMap);
    );
);

genericProjection(Ring) := opts -> (R1) -> (
    return genericProjection(1, R1, opts);
);

projectionToHypersurface = method(Options => optProjectionToHypersurface);
-*
projectionToHypersurface(Ideal) := opts -> (I1) -> (
        local c1;
        if (opts.Codimension === null) then (
            c1 = codim I1;
        ) else (c1 = opts.Codimension);
        return genericProjection(c1-1, I1, Homogeneous => opts.Homogeneous, MaxCoordinatesToReplace => opts.MaxCoordinatesToReplace, Replacement => opts.Replacement, Verbose=>opts.Verbose);
);*-

projectionToHypersurface(Ring) := opts -> (R1) -> (
    LO := projectionToHypersurface(ideal R1, opts);
    phi := map(R1, (source (LO#0))/(LO#1), matrix (LO#0) );
    return (phi, source phi);
);

--projectionToHypersurfaceV2 = method(Options=>optProjectionToHypersurface);

projectionToHypersurface(Ideal) := opts -> (I1) -> (
    local c1;
    R1 := ring I1;
    if (class R1 =!= PolynomialRing) then error "projectionToHypersurface:  expected an ideal in a polynomial ring.";
    
    if (opts.Codimension === null) then (
        c1 = codim I1;
    ) else (c1 = opts.Codimension);
    if (c1 <= 1) then return (map(R1, R1), I1); --its already a hypersurface
    if (c1 == infinity) then return (map(R1, R1), I1); --it's the unit ideal, and hence also a hypersurface
    n1 := c1-1;
    
    --build the target ring
    kk:=coefficientRing R1;
    myVars := drop(gens R1, n1);
    Rs := kk(monoid[myVars]);
    d2 := #myVars;
    
    --build the replacement map
    local genList;
    if (opts.Replacement == Binomial) then (
        genList = getRandomLinearForms(R1, {0, max(d2 - opts.MaxCoordinatesToReplace, 0), 0, min(d2, opts.MaxCoordinatesToReplace), 0, 0}, Homogeneous => opts.Homogeneous, Verbose=>opts.Verbose, Verify=>true); )
    else if (opts.Replacement == Trinomial) then (
        genList = getRandomLinearForms(R1, {0, max(d2 - opts.MaxCoordinatesToReplace, 0), 0, 0, min(d2, opts.MaxCoordinatesToReplace), 0}, Homogeneous => opts.Homogeneous, Verbose=>opts.Verbose, Verify=>true); )
    else if (opts.Replacement == Monomial) then (
        genList = getRandomLinearForms(R1, {0, d2, 0, 0, 0, 0}, Homogeneous => opts.Homogeneous, Verbose=>opts.Verbose, Verify=>true); )        
    else if (opts.Replacement == Full) then (
        genList = getRandomLinearForms(R1, {0, max(d2 - opts.MaxCoordinatesToReplace, 0), 0, 0, 0, min(d2, opts.MaxCoordinatesToReplace)}, Homogeneous => opts.Homogeneous, Verbose=>opts.Verbose, Verify=>true); );
--    genList = random apply(genCount, t -> if (t < opts.MaxCoordinatesToReplace) then replacementFunction(genList#t) else genList#t);
    psi := map(R1, Rs, genList);
    myMap := map(R1/I1, Rs, genList);
    J1 := ker(myMap, SubringLimit=>1);
    return (psi, J1);
);


-*
projectionToHypersurface(Ideal) := opts -> (I1) -> (
	local c1;
	if (opts.Codimension === null) then (
		c1 = codim I1;
	) else (c1 = opts.Codimension);
	local curMap;
	tempList := genericProjection(I1, Homogeneous => opts.Homogeneous, MaxCoordinatesToReplace => opts.MaxCoordinatesToReplace);
	assert(target (tempList#0) === ring I1);
	if (c1 == 2) then (
		return tempList; --if we are done, stop
	);
	assert(source (tempList#0) === ring (tempList#1));
	--otherwise recurse
	tempList2 := projectionToHypersurface(tempList#1, Hoxmogeneous => opts.Homogeneous, MaxCoordinatesToReplace => opts.MaxCoordinatesToReplace, Codimension=>c1-1);
	assert(target(tempList2#0) === ring (tempList#1));
	return ((tempList#0)*(tempList2#0), tempList2#1);
);
*-

--this function just switches one strategy in an option table for another
switchStrategy := (opts, newStrat) -> (
    tempHashTable := new MutableHashTable from opts;
    tempHashTable#Strategy = newStrat;
    return new OptionTable from tempHashTable;
);

verifyPoint = method(Options => optRandomPoints);

verifyPoint(List, List) := opts -> (finalPoint, idealList) ->(
    verifyPoint(finalPoint, ideal idealList)
);

verifyPoint(List, Ideal) := opts -> (finalPoint, I1) -> (
    if (opts.Homogeneous) then( 
        if (all(finalPoint, t -> t == 0)) then return false;
    );
    if (#finalPoint > 0) then (
        S1 := ring finalPoint#0;
        if (any(first entries gens I1, tt -> evalAtPoint(S1, tt, finalPoint) != 0)) then (
            if (opts.Verbose or debugLevel > 0) then print "verifyPoint: found a point that is not on the variety, if not using a projection strategy, this may indicate a problem.";
            return false;
        );
    )
    else return false;
    return true;
);


saturateInGenericCoordinates=method(Options => {Replacement=>Binomial})
saturateInGenericCoordinates(Ideal):= opts -> I1 -> (
    local x;
    S1 := ring I1;
    --x:=random(0, S1)*random(1, S1) + random(1, S1);
    if (opts.Replacement == Monomial) then x=getRandomLinearForms(S1, {0,1,0,0, 0,0}, Homogeneous => true)
    else if (opts.Replacement == Binomial) then x=getRandomLinearForms(S1, {0,0,0,1, 0,0}, Homogeneous => true)
    else if (opts.Replacement == Trinomial) then x=getRandomLinearForms(S1, {0,0,0,0, 1,0}, Homogeneous => true)
    else if (opts.Replacement == Full) then x=getRandomLinearForms(S1, {0,0,0,0, 0,1}, Homogeneous => true)
    else x=getRandomLinearForms(S1, {0,0,0,1, 0,0}, Homogeneous => true);
    saturate(I1,ideal x)
)


--The following gets a list of random forms in a ring.  You specify how many.  
--if Verify is true, it will check to for linear independence of the monomial, binomial and randForms 
getRandomLinearForms = method(Options => {Verify => false, Homogeneous => false, Verbose=>false});
getRandomLinearForms(Ring, List) := opts -> (R1, L1) ->(
    if (opts.Verbose) or (debugLevel > 0) then print concatenate("getRandomLinearForms: starting, options:", toString(L1));
    constForms := L1#0;
    monomialForms := L1#1;
    trueMonomialForms := L1#2; --forced to be monomial whether or not Homogeneous is true
    binomialForms := L1#3; 
    trinomialForms := L1#4;
    randForms := L1#5;
    formList := {}; 
    genList := gens R1;
    d := #genList;
    --tempList is used if Verify => true, it tries to maximize linear independence of monomial and binomial forms,
    --this is important if you are trying to verify some ring map is actually surjective 
    if (d <= 0) then (
        
    );
    tempList := random genList;
    if (opts.Verify) then (
        if (#tempList < monomialForms + trueMonomialForms + binomialForms) then (tempList = tempList | apply(monomialForms + trueMonomialForms + binomialForms - #tempList, i->(genList)#(random d)));
    );
    if (opts.Homogeneous) then (
        if (opts.Verbose) or (debugLevel > 0) then print "getRandomLinearForms: generating homogeneous forms.";
        --monomialForms x,y,z
        if (opts.Verify) then (formList = formList | apply(monomialForms, i -> (tempList)#i);)
        else (formList = formList | apply(monomialForms, i -> (genList)#(random(d))););
        --true monomialForms, which is the same as monomial forms in the homogeneous case
        if (opts.Verify) then (formList = formList | apply(trueMonomialForms, i -> (tempList)#(i+monomialForms));)
        else (formList = formList | apply(trueMonomialForms, i -> (genList)#(random(d))););
        --binomial forms, x+by
        if (opts.Verify) then (formList = formList | apply(binomialForms, i -> (tempList)#(i+monomialForms+trueMonomialForms) + (random(0, R1))*(genList)#(random(d)));) 
        else (formList = formList | apply(binomialForms, i -> (genList)#(random(d)) + (random(0, R1))*(genList)#(random(d))););
        --trinomial forms, x+by+cz
        if (opts.Verify) then (formList = formList | apply(trinomialForms, i -> (tempList)#(i+monomialForms+trueMonomialForms) + (random(0, R1))*(genList)#(random(d))  + (random(0, R1))*(genList)#(random(d))  );) 
        else (formList = formList | apply(trinomialForms, i -> (genList)#(random(d)) + (random(0, R1))*(genList)#(random(d)) + (random(0, R1))*(genList)#(random(d))  ););
        --random forms
        formList = formList | apply(randForms, i-> random(1, R1));
    )
    else(
        if (opts.Verbose) or (debugLevel > 0) then print "getRandomLinearForms: generating non-homogeneous forms.";
        --monomial forms, x+a
        if (opts.Verify) then (formList = formList | apply(monomialForms, i -> random(0, R1) + (tempList)#i);)
        else (formList = formList | apply(monomialForms, i -> random(0, R1) + (genList)#(random(d))););
        --true monomial forms, x, y, z
        if (opts.Verify) then (formList = formList | apply(trueMonomialForms, i -> (tempList)#(i+monomialForms));)
        else (formList = formList | apply(trueMonomialForms, i -> (genList)#(random(d))););
        --binomial forms, x+by+c        
        if (opts.Verify) then (formList = formList | apply(binomialForms, i -> random(0, R1) + (tempList)#(i+monomialForms+trueMonomialForms) + (random(0, R1))*(genList)#(random(d)));) 
        else (formList = formList | apply(binomialForms, i -> random(0, R1) + (genList)#(random(d)) + (random(0, R1))*(genList)#(random(d))););
        --trinomial forms x+by+cz + d
        if (opts.Verify) then (formList = formList | apply(trinomialForms, i -> random(0, R1) + (tempList)#(i+monomialForms+trueMonomialForms) + (random(0, R1))*(genList)#(random(d))  + (random(0, R1))*(genList)#(random(d)));) 
        else (formList = formList | apply(trinomialForms, i -> random(0, R1) + (genList)#(random(d)) + (random(0, R1))*(genList)#(random(d)) + (random(0, R1))*(genList)#(random(d))));
        --random forms
        formList = formList | apply(randForms, i->random(0, R1) + random(1, R1));
    );
    if (opts.Verify) and (#formList > 0) then ( --if we are checking our work
        J1 := jacobian ideal formList;
        val := min(d, #formList);
        if (rank J1 < val) then ( 
            if (opts.Verbose) or (debugLevel > 0) then print "getRandomLinearForms: forms were not random enough, trying again recursively.";            
            return getRandomLinearForms(R1, L1, opts);
        );
    );
    formList = formList | apply(constForms, i -> random(0, R1));

    return random formList;
);



randomPointViaDefaultStrategy = method(Options => optRandomPoints);
randomPointViaDefaultStrategy(ZZ, Ideal) := List => opts -> (n1, I1) -> (
    local fieldSize;
    tempPtsList := {};
    runDecomp := true;
    runMult1 := false;
    runMult2 := true;
    pointsList := {}; --a list of points to output
    if (isHomogeneous I1) and (not (opts.DecompositionStrategy === Decompose)) then (
        if (opts.DecompositionStrategy === MultiplicationTable) then (
            runDecomp = false;
            if (runMult1) then print "randomPointViaDefaultStrategy: running no Decompose.";
        );
        if (dim ring I1 <= 15) then (runMult1 = true; runMult2 = false);
        if (runMult1) and (opts.Verbose) then print "randomPointViaDefaultStrategy: running multiplication tables first.";
        if (runMult2) and (opts.Verbose) then print "randomPointViaDefaultStrategy: running decompose first.";
    )
    else (
        if (opts.Verbose) or (debugLevel > 0) then print "randomPointViaDefaultStrategy: running no multiplication tables.";
        runMult1 = false;
        runMult2 = false;
    );
    homog := (isHomogeneous I1) and (opts.DecompositionStrategy === MultiplicationTable);
    d1 := dim ring I1;
    d1Half := ceiling(d1/2);
    d1Half2 := d1 - d1Half;    
    
    --make sure we aren't doing something stupid...
    if (isHomogeneous I1) and (isDimAtMost(0, I1) === true) then (
        if (opts.Homogeneous == true) then (
            if opts.Verbose then print "randomPointViaDefaultStrategy: empty set";
            return {}
        ) 
        else( 
            if opts.Verbose then print "randomPointViaDefaultStrategy: origin";
            return {idealToPoint(ideal first entries vars ring I1)};
        );
    );

    if (opts.Verbose) or (debugLevel > 0) then print "randomPointViaDefaultStrategy(step 0): trying a quick brute force with 20 attempts.";
    pointsList = pointsList | randomPointsBranching(n1 - #pointsList, I1, 
            opts++{ Strategy=>BruteForce, PointCheckAttempts => 20 }
        );
    if (#pointsList >= n1) then return pointsList;

    if (runMult1) then (
        if (opts.Verbose) or (debugLevel > 0) then print "randomPointViaDefaultStrategy(step 1): attempting linear intersection via multiplication table with binomials.";
        pointsList = pointsList | linearIntersectionNew(n1 - #pointsList, I1, 
            Homogeneous => opts.Homogeneous,          
            Replacement => Binomial,        
            ExtendField => opts.ExtendField,
            PointCheckAttempts => 5*(n1 - #pointsList),
            DecompositionStrategy => MultiplicationTable,        
            DimensionFunction => opts.DimensionFunction,
            Verbose => false);
        if (#pointsList >= n1) then return pointsList;
    );
    

    if (runDecomp) then (
        if (opts.Verbose) or (debugLevel > 0) then print "randomPointViaDefaultStrategy(step 1a): attempting linear intersection with monomials.";
        tempPtsList = linearIntersectionNew(n1 - #pointsList, I1, 
            Homogeneous => opts.Homogeneous,          
            Replacement => Monomial,        
            ExtendField => opts.ExtendField,
            PointCheckAttempts => 5*(n1 - #pointsList),
            DecompositionStrategy => Decompose,        
            DimensionFunction => opts.DimensionFunction,
            Verbose => false);
        if (tempPtsList === null) then return {}; --this returned there are no points at all
        pointsList = pointsList | tempPtsList;
        if (#pointsList >= n1) then(
            if (opts.Verbose) or (debugLevel > 0) then print "randomPointViaDefaultStrategy(step 1a):success";
            return pointsList;   
        ); 
    );

    if (runMult2) then (
        if (opts.Verbose) or (debugLevel > 0) then print "randomPointViaDefaultStrategy(step 1b): attempting linear intersection via multiplication table with binomials.";
        pointsList = pointsList | linearIntersectionNew(n1 - #pointsList, I1, 
            Homogeneous => opts.Homogeneous,          
            Replacement => Binomial,        
            ExtendField => opts.ExtendField,
            PointCheckAttempts => 5*(n1 - #pointsList),
            DecompositionStrategy => MultiplicationTable,        
            DimensionFunction => opts.DimensionFunction,
            Verbose => false);
        if (#pointsList >= n1) then return pointsList;
    );
    

    if (runMult1) then (
        if (opts.Verbose) or (debugLevel > 0) then print "randomPointViaDefaultStrategy(step 2): attempting linear intersection via multiplication table with binomials + trinomials.";
        pointsList = pointsList | linearIntersectionNew(n1 - #pointsList, I1, 
            Homogeneous => opts.Homogeneous,          
            Replacement => {0,0,0,d1Half, d1Half2, 0},        
            ExtendField => opts.ExtendField,
            PointCheckAttempts => 3*(n1 - #pointsList),
            DecompositionStrategy => MultiplicationTable,        
            DimensionFunction => opts.DimensionFunction,
            Verbose => false);
        if (#pointsList >= n1) then return pointsList;
    );
    
    if (runDecomp) then (
        if (opts.Verbose) or (debugLevel > 0) then print "randomPointViaDefaultStrategy(step 2a): attempting linear intersection with monomials + binomials.";
        tempPtsList = linearIntersectionNew(n1 - #pointsList, I1, 
            Homogeneous => opts.Homogeneous,          
            Replacement => {0,d1Half, 0, d1Half2, 0,0},        
            ExtendField => opts.ExtendField,
            PointCheckAttempts => 5*(n1 - #pointsList),
            DecompositionStrategy => Decompose,        
            DimensionFunction => opts.DimensionFunction,
            Verbose => false);
        if (tempPtsList === null) then return {}; --this returned there are no points at all
        pointsList = pointsList | tempPtsList;
        if (#pointsList >= n1) then return pointsList;     
    );

    if (runMult2) then (
        if (opts.Verbose) or (debugLevel > 0) then print "randomPointViaDefaultStrategy(step 2b): attempting linear intersection via multiplication table with binomials + trinomials.";
        pointsList = pointsList | linearIntersectionNew(n1 - #pointsList, I1, 
            Homogeneous => opts.Homogeneous,          
            Replacement => {0,0,0,d1Half, d1Half2, 0},        
            ExtendField => opts.ExtendField,
            PointCheckAttempts => 3*(n1 - #pointsList),
            DecompositionStrategy => MultiplicationTable,        
            DimensionFunction => opts.DimensionFunction,
            Verbose => false);
        if (#pointsList >= n1) then return pointsList;
    );

    if (runMult1) then (
        if (opts.Verbose) or (debugLevel > 0) then print "randomPointViaDefaultStrategy(step 3): attempting linear intersection via multiplication table with trinomials.";
        pointsList = pointsList | linearIntersectionNew(n1 - #pointsList, I1, 
            Homogeneous => opts.Homogeneous,          
            Replacement => Trinomial,        
            ExtendField => opts.ExtendField,
            PointCheckAttempts => 2*(n1 - #pointsList),
            DecompositionStrategy => MultiplicationTable,        
            DimensionFunction => opts.DimensionFunction,
            Verbose => false);
        if (#pointsList >= n1) then return pointsList;
    );
    
    if (runDecomp) then (
        if (opts.Verbose) or (debugLevel > 0) then print "randomPointViaDefaultStrategy(step 3a): attempting linear intersection with binomials.";
        tempPtsList =  linearIntersectionNew(n1 - #pointsList, I1, 
            Homogeneous => opts.Homogeneous,          
            Replacement => Binomial,        
            ExtendField => opts.ExtendField,
            PointCheckAttempts => 5*(n1 - #pointsList),
            DecompositionStrategy => Decompose,        
            DimensionFunction => opts.DimensionFunction,
            Verbose => false);
        if (tempPtsList === null) then return {}; --this returned there are no points at all
        pointsList = pointsList | tempPtsList;
        if (#pointsList >= n1) then return pointsList;
    );

    if (runMult2) then (
        if (opts.Verbose) or (debugLevel > 0) then print "randomPointViaDefaultStrategy(step 3b): attempting linear intersection via multiplication table with trinomials.";
        pointsList = pointsList | linearIntersectionNew(n1 - #pointsList, I1, 
            Homogeneous => opts.Homogeneous,          
            Replacement => Trinomial,        
            ExtendField => opts.ExtendField,
            PointCheckAttempts => 2*(n1 - #pointsList),
            DecompositionStrategy => MultiplicationTable,        
            DimensionFunction => opts.DimensionFunction,
            Verbose => false);
        if (#pointsList >= n1) then return pointsList;
    );    

    if (runDecomp) then (
        if (opts.Verbose) or (debugLevel > 0) then print "randomPointViaDefaultStrategy(step 4): attempting linear intersection with trinomials.";
        tempPtsList =  linearIntersectionNew(n1 - #pointsList, I1, 
            Homogeneous => opts.Homogeneous,          
            Replacement => Trinomial,        
            ExtendField => opts.ExtendField,
            PointCheckAttempts => 5*(n1 - #pointsList),
            DecompositionStrategy => Decompose,        
            DimensionFunction => opts.DimensionFunction,
            Verbose => false);
        if (tempPtsList === null) then return {}; --this returned there are no points at all
        pointsList = pointsList | tempPtsList;
        if (#pointsList >= n1) then return pointsList;
    );
    
    if (runMult1) then (
        if (opts.Verbose) or (debugLevel > 0) then print "randomPointViaDefaultStrategy(step 5): attempting linear intersection via multiplication table with full.";
        pointsList = pointsList | linearIntersectionNew(n1 - #pointsList, I1, 
            Homogeneous => opts.Homogeneous,          
            Replacement => Full,        
            ExtendField => opts.ExtendField,
            PointCheckAttempts => 2*(n1 - #pointsList),
            DecompositionStrategy => MultiplicationTable,        
            DimensionFunction => opts.DimensionFunction,
            Verbose => false);
        if (#pointsList >= n1) then return pointsList;
    );

    if (runDecomp) then (
        if (opts.Verbose) or (debugLevel > 0) then print "randomPointViaDefaultStrategy(step 5a): attempting linear intersection with full.";
        tempPtsList = linearIntersectionNew(n1 - #pointsList, I1, 
            Homogeneous => opts.Homogeneous,          
            Replacement => Full,        
            ExtendField => opts.ExtendField,
            PointCheckAttempts => 5*(n1 - #pointsList),
            DecompositionStrategy => Decompose,        
            DimensionFunction => opts.DimensionFunction,
            Verbose => false);
        if (tempPtsList === null) then return {}; --this returned there are no points at all
        pointsList = pointsList | tempPtsList;
        if (#pointsList >= n1) then return pointsList;
    );

    if (runMult2) then (
        if (opts.Verbose) or (debugLevel > 0) then print "randomPointViaDefaultStrategy(step 5b): attempting linear intersection via multiplication table with full.";
        pointsList = pointsList | linearIntersectionNew(n1 - #pointsList, I1, 
            Homogeneous => opts.Homogeneous,          
            Replacement => Full,        
            ExtendField => opts.ExtendField,
            PointCheckAttempts => 2*(n1 - #pointsList),
            DecompositionStrategy => MultiplicationTable,        
            DimensionFunction => opts.DimensionFunction,
            Verbose => false);
    );
    if (#pointsList >= n1) then return pointsList;
    if opts.Verbose then print "randomPointViaDefaultStrategy: failure";
    return pointsList;
);



linearIntersectionNew = method(Options => optRandomPoints);
linearIntersectionNew(ZZ, Ideal) := opts -> (n1, I1) -> (
    --this code is for the non-homogeneous case
    --the idea in this code is simple.  Extend the field.  Intersect with binomials.  
    --Then find the point, either by decompose or with a multiplication table
    --If this finds no points on a zero dimensional scheme, it returns null (and not just the empty scheme)
    local J2;
    local I3;
    local myDeg;
    local S2;
    local I2;
    local phi1;
    local newS2;
    local psi;
    local j;
    local l; --a linear space
    local m2;  --a point, in an extended field
    local finalPoint; --the point to be returned    
    local J2;
    local ptList; --list of points, before extending the field.
    local sortedPtList; --list of points, before extending the field.
    local newPtList; --list of points after extending the field
    local checks;
    local newPts; --new points as found by the multiplication table helper function
    local d;
    local L2;
    local workingIdeal;
    local fastDim0;
    local fastDim1;

    if (opts.DimensionFunction === null) then (
        fastDim0 = Ii -> (isDimAtMost(0, Ii) === true);
        fastDim1 = Ii -> (isDimAtMost(1, Ii) === true);
    )
    else (
        fastDim0 = Ii -> ((opts.DimensionFunction)(Ii) == 0);
        fastDim1 = Ii -> ((opts.DimensionFunction)(Ii) == 1);
    );
    returnPointsList := {};
    if (opts.PointCheckAttempts >= 1 ) then checks = opts.PointCheckAttempts else if (opts.ExtendField) then  checks = 3*n1+3 else checks = 3*n1+3;  

    if (opts.Verbose) or (debugLevel > 1) then print "linearIntersectionNew: starting";
    
    S1 := ring I1;
    m := getFieldSize coefficientRing S1;
    pp := char ring I1;
    if (opts.ExtendField) then ( 
        d = ceiling(log_pp(max(m, 100)) + 0.5); --this should force a bigger field extension
        curD := floor(log_pp(m) + 0.5);--current degree
        if opts.Verbose or debugLevel > 0 then print "linearIntersectionNew: Extending the field";
        if opts.Verbose or debugLevel > 0 then print ("linearIntersectionNew: New field size is " | toString(pp) | "^" | toString(d) | " = " | toString(pp^(d)) );
        d = ceiling(d/curD)*d; --make sure our extension degree is a multiple of the old degree
        (S2, phi1) = fieldBaseChange(S1, GF(pp, d));
        I2 = phi1(I1);    
    )
    else(
        d = floor(log_pp(m) + 0.5); --the current degree, for later storage
        S2 = S1;
        I2 = I1;
    );
    i := dim S1;
    k:= 0;
    

    --this flag is set to true if we going to use MultiplicationTables
    homogFlag := false;    
    if (opts.Homogeneous) then (
        if (opts.DecompositionStrategy === MultiplicationTable) then (
            homogFlag = isHomogeneous(I1);
        )
        else if (opts.DecompositionStrategy === Decompose) then (
            homogFlag = false;
        )
        else (
            homogFlag = isHomogeneous(I1) and (dim S1 <= 15);
        );
    );
    if (opts.Verbose) then (
        if (homogFlag) then print "linearIntersectionNew: using MultiplicationTable";
        if (not homogFlag) then print "linearIntersectionNew: using Decompose";
    )    ;
    
    --depending on what was passed, we choose a list of 
    if (class opts.Replacement === List) then (
        if (not (sum opts.Replacement == i)) and (opts.Verbose) then print "linearIntersectionNew: Warning, you passed a replacement scheme but the terms did not add up to the ambient dimension."; 
        L2 = apply(checks, t-> getRandomLinearForms(S2, opts.Replacement, Homogeneous=>homogFlag, Verify=>true)); --a list of linear forms
    )
    else if (opts.Replacement === Monomial) then (
        L2 = apply(checks, t -> getRandomLinearForms(S2, {0, i, 0, 0, 0, 0}, Homogeneous=>homogFlag, Verify=>true));
    )
    else if (opts.Replacement === Binomial) then (
        L2 = apply(checks, t -> getRandomLinearForms(S2, {0, 0, 0, i, 0, 0},Homogeneous=>homogFlag, Verify=>true));
    )
    else if (opts.Replacement === Trinomial) then (
        L2 = apply(checks, t -> getRandomLinearForms(S2, {0, 0, 0, 0, i, 0},Homogeneous=>homogFlag, Verify=>true));
    )
    else if (opts.Replacement === Full) then (
        L2 = apply(checks, t -> getRandomLinearForms(S2, {0, 0, 0, 0, 0, i}, Homogeneous=>homogFlag, Verify=>true));
    )
    else (
        L2 = apply(checks, t-> getRandomLinearForms(S2, {0,max(0, i-t),0,min(t, i), 0,0}, Homogeneous=>homogFlag, Verify=>true)); --a list of linear forms, some monomial, some binomial
    ); --now we have picked the linear forms, we begin our loop
    while (i >= 0) do ( --i loops over dimension
        if opts.Verbose or debugLevel > 0 then print("linearIntersectionNew: Trying intersection with a linear space of dimension " | toString(dim S2 - i));        
        J2 = apply(L2, l -> ideal(l) + I2);
        k = checks-1; --this should change to start at checks-1 and loop backwards
        while (k >= 0) and (#returnPointsList < n1) do ( --this loops over the linear spaces we are working with
            if opts.Verbose or debugLevel > 0 then print("linearIntersectionNew: trying linear intersection #" | toString(k));
            workingIdeal = J2#k;
            
            ---trying some things to speed stuff up.            
            curFlag := false;
            if (homogFlag) then (
                --print (dim workingIdeal);
                if not (isDimAtMost(0, workingIdeal) === true) then --do a fast attempt to show the dimension is bounded above by 0, in which case don't even try to saturate
                (
                    workingIdeal = saturateInGenericCoordinates workingIdeal;
                    curFlag = (workingIdeal != ideal 1_S2); 
                );                
            )
            else(
               curFlag = (workingIdeal != ideal 1_S2); 
            );
            --end of attempt to speed things up

            --the old code is below
            --if (homogFlag) then workingIdeal = saturate workingIdeal; --make this saturation faster
            --if (homogFlag) then workingIdeal = saturateInGenericCoordinates workingIdeal; --make this saturation faster, maybe call isDimAtMost(0, workingIdeal) here instead, or at least first
            if (curFlag) and (#returnPointsList < n1) then (--we found a point                
                --i = -1; --stop the exterior loop, we found the dimension
                if opts.Verbose or debugLevel > 0 then print("linearIntersectionNew: We found something.");
                oldPtCt := #returnPointsList;                
                if ((not homogFlag) and ((fastDim0(workingIdeal) == true))) then (--if we are using decompose
                    if opts.Verbose or debugLevel > 0 then print("linearIntersectionNew: We found at least one point");
                    
                    ptList = random decompose trim (workingIdeal);                        
                    if opts.Verbose or debugLevel > 0 then print("linearIntersectionNew: We found " | toString(#ptList) | " points.");
                    j=0;
                    sortedPtList = sort apply(#ptList, t -> {0, degree (ptList#t), t});                    
                    while (j < #ptList) and (#returnPointsList < n1) and (sortedPtList#j#0 == 0) do ( --loops over the points we decomposed
                        myDeg = sortedPtList#j#1;                
                        if opts.Verbose or debugLevel > 0 then print("linearIntersectionNew: Looking at a point of degree " | toString(myDeg));
                        if (myDeg == 1) then (
                            finalPoint = idealToPoint(ptList#(sortedPtList#j#2));                                                
                            if (verifyPoint(finalPoint, I2, opts)) then returnPointsList = append(returnPointsList, finalPoint);
                        )
                        else if (opts.ExtendField) and (not (null === conwayPolynomial(pp, d*myDeg)))  then (
                            if (debugLevel > 0) or (opts.Verbose) then print "linearIntersectionNew:  extending the field.";
                            psi = (extendFieldByDegree(myDeg, S2))#1;
                            I3 = psi(I2);
                            newS2 = target psi;
                            m2 = psi(ptList#j);
                            newPtList = random decompose(m2); --make sure we are picking points randomly from this decomposition
                            --since these points are going to be conjugate, we only pick 1.                      
                            if (#newPtList > 0) then ( 
                                finalPoint = idealToPoint(newPtList#0);
                                --finalPoint =  apply(idealToPoint(newPtList#0), s -> sub(s, target phi));
                                if (verifyPoint(finalPoint, I3, opts)) then (
                                    returnPointsList = append(returnPointsList, finalPoint);
                                );                     
                            ); 
                        )
                        else if (opts.ExtendField) then (
                            if (debugLevel > 0) or (opts.Verbose) then print "linearIntersectionNew: Macaulay2 cannot handle a field extension this large, moving to the next point.";
                        );
                        j = j+1;
                    );                                                                                   
                    if (i == 0) and (#returnPointsList == 0) then (
                        if (debugLevel > 0) or (opts.Verbose) then print "linearIntersectionNew: this is a <= 0 dimensional variety with no points.";
                        return null;
                    );
                )
                else if homogFlag and (fastDim1(workingIdeal) == true) then (--we should use MultiplicationTable to do the factoring
                    newPts = multiplicationTableInternal(n1 - #returnPointsList, I2, workingIdeal, sub(ideal(L2#k), S2), ExtendField => opts.ExtendField, Verbose=>opts.Verbose);
                    returnPointsList = returnPointsList | newPts;                                       
                );  
                if (debugLevel > 0) or (opts.Verbose) then print("linearIntersectionNew: added " |toString(#returnPointsList - oldPtCt) | " points to the list");
                L2 = drop(L2, {k,k});
                checks = checks-1; --we have fewer linear forms now      
                if (debugLevel > 0) or (opts.Verbose) then print "linearIntersectionNew: Removing linear space from the list, this one found something.";
            );
            if (#returnPointsList >= n1) then return returnPointsList;
            k = k-1;
        );
        L2 = apply(checks, t->drop(L2#t, 1)); --drop something for next run
        i = i-1;        
    );
    return returnPointsList;
)

multiplicationTableInternal = method(Options=>{ExtendField => false, Verbose=>false});
multiplicationTableInternal(ZZ, Ideal, Ideal, Ideal) := opts->(n1, I2, workingIdeal, linearSpace) -> (
    local newPt;
    returnPointsList := {};
    S2 := ring I2;
    if (degree workingIdeal == 1) then (--we don't have to do anything
        newPt = flatten (entries syz transpose jacobian workingIdeal);
        if (#newPt > dim S2) then error "multiplicationTableInternal: What is going on?";
        returnPointsList = append(returnPointsList, newPt);    
    )
    else ( --do the multiplication table thing
        r:=degree ideal last (entries gens gb workingIdeal)_0;
        b1 :=basis(r+1,S2^1/workingIdeal); 
        b2 :=basis(r+2,S2^1/workingIdeal);
        j := dim S2 - #(first entries gens linearSpace) - 1; --what size linear space did we intersect with
        if (opts.Verbose) then print ("multiplicationTableInternal: linear space codim is " | toString(j) );
        --| "," | toString(dim linearSpace) | " deg " | toString(degree workingIdeal));    
        xx:=(support (vars S2%(linearSpace)))_{j-1,j}; 
        --the above should be written to choose something more intelligent, like a random variable instead of the last one
        m0:=contract(transpose matrix entries b2,matrix entries((xx_0*b1)%workingIdeal));  
        m1:=contract(transpose matrix entries b2,matrix entries((xx_1*b1)%workingIdeal));
        M:=map(S2^(rank target m0),S2^{rank source m0:-1},xx_0*m1-xx_1*m0);

        DetM:=(M^{0}*syz M^{1..rank source M-1})_(0,0);--fake determinant computation  
        if (not DetM == 0) then (
            h:= factor DetM;
            count := 0;
            while (count < #h) and (#returnPointsList < n1) do (
                myH := first (h#count);
                if (degree myH >= degree(0_S2)) and (degree myH == degree first first entries vars S2) then (--check to see if we have a degree 1 factor
                    --pt:=radical saturate((ideal myH)+workingIdeal); --lift to a higher dimensional space
                    pt:=radical saturateInGenericCoordinates((ideal myH)+workingIdeal); --lift to a higher dimensional space
                    if (degree pt == 1) then (
                        newPt = flatten (entries syz transpose jacobian pt);
                        verifyPoint(newPt, I2);
                        if (#newPt > dim S2) then error "multiplicationTableInternal: What is going on?";
                        returnPointsList = append(returnPointsList, newPt);                    
                    )
                    else(
                        if (opts.Verbose) then print "multiplicationTableInteral: we don't handle higher degree points via multiplicationTable yet";
                    );
                )                
                else(
                    if (opts.Verbose) then print "multiplicationTableInteral: we don't handle higher degree points via multiplicationTable yet";
                );            
                count = count+1;
            );
        );
    );
    return returnPointsList; --return what we have done
);






getNextValidFieldSize = method();
getNextValidFieldSize(ZZ, ZZ, ZZ) := (pp, d, targetSize) -> (
    i := 1;
    while (pp^(i*d) < targetSize) do (
        i = i+1;
    );
    i
);



--better canceling provided by Dan Grayson
cancel = task -> (
     << "cancelling task " << task << endl;
     cancelTask task; 
     while true do (
	  if isCanceled task then (<< "cancelled task terminated " << task << endl; break);
	  if isReady task then (taskResult task ; << "cancelled task finished " << task << endl; break);
	  nanosleep(10000000);
	  );
     << "cancelled task " << task << endl;
)

dimViaBezout(Ideal) := opts-> I1 -> (    
    S1 := ring I1;
    if not (class S1 === PolynomialRing) then error "dimViaBezout: Expected an ideal in a polynomial ring.";
    m := getFieldSize coefficientRing S1;
    ambD := dim S1;
    local attempts;
    local tr;
    local tempResult;
    local homog;
    local val;
    local minFieldSize;   
    local replacementList;

    if (opts.Homogeneous) then (homog = isHomogeneous I1) else (homog = opts.Homogeneous);
    effAmbD := ambD;
    if (homog) then effAmbD = effAmbD - 1;

    if (class opts.Replacement === List) then (
        if not (sum (opts.Replacement) == effAmbD) then error "dimViaBezout: sum of replacement terms does not add up to the effective dimension";
        replacementList = opts.Replacement;
    )
    else if (opts.Replacement === Monomial) then (
        replacementList = {0, effAmbD, 0, 0, 0, 0};
    )
    else if (opts.Replacement === Binomial) then (
        replacementList = {0, 0, 0, effAmbD, 0, 0};
    )
    else if (opts.Replacement === Trinomial) then (
        replacementList = {0, 0, 0, 0, effAmbD, 0};
    )
    else if (opts.Replacement === Full) then (
        replacementList = {0, 0, 0, 0, 0, effAmbD};
    )
    else (
        error "dimViaBezout: not a valid replacement option";
    );

    
    pp := char ring I1;
    d := floor(log_pp(m) + 0.5);
    if (opts.MinimumFieldSize === null) then (
        --one bad case is if I1 defines a point.  
        --Then we need to worry about one of the linear forms containing the point.  
        --The chance a given hypersurface contains a point is probably on the order of 1/p.  
        --So if we want say a 99% chance of things going well, we need 1 - ((p-1)/p)^d <= 1/1000.
        --here I'm using d for dim (instead of ambD)
        --(p-1)/p >= (1 -(1/100))^(1/d)
        --1-1/p >= (1 -(1/100))^(1/d)
        --1 - (1 -(1/100))^(1/d) >= 1/p
        --p >= 1/(1 - (1 -(1/100))^(1/d))
        --This basically turns out to be very similar to p >= d*100 after simplification
        minFieldSize = ambD*100;        
    )
    else (
        minFieldSize = opts.MinimumFieldSize; --or the user can specify it
    );
    i := getNextValidFieldSize(pp, d, minFieldSize);
    --if (opts.DimensionIntersectionAttempts === null) then (attempts = ceiling(log_10(1 + 10000/(pp^(i*d))))) else (attempts = opts.DimensionIntersectionAttempts;);
    if (opts.DimensionIntersectionAttempts === null) then (
        if (homog) then attempts = 3 else attempts = 3;
    )
    else(
        attempts = opts.DimensionIntersectionAttempts;
    );
    
    if opts.Verbose or debugLevel > 0 then print ("dimViaBezout: Checking each dimension " | toString(attempts) | " times.");
    if (m >= minFieldSize) then (
        if opts.Verbose or debugLevel > 0 then print ("dimViaBezout: field size is big enough, not extending.");
        --The following is code for multithreading, if that is fixed.
        -*
        backtrace=false;
        t1 := createTask(myI -> (backtrace=false; return dimViaBezoutNonhomogeneous myI), (I1, Verbose=>false, DimensionIntersectionAttempts=>attempts));
        t2 := createTask(myI -> (backtrace=false; return dim myI), (I1));
        schedule t1;
        schedule t2;
        r1 := isReady(t1);
        r2 := isReady(t2);
        if opts.Verbose or debugLevel > 0 then print ("dimViaBezout:  starting threads, one classical dim, one probabilistic dim ");
        while (r1==false and r2==false) do ( nanosleep(100000); r1 = isReady(t1); r2 = isReady(t2););
        if opts.Verbose or debugLevel > 0 then print ("dimViaBezout:  found an answer" );
        if (r2) then (
            tr = taskResult(t2);
            if opts.Verbose or debugLevel > 0 then print ("dimViaBezout:  classical dim finished first: " | toString(tr) );            
            cancel t1;                       
            return tr;
        )
        else if (r1) then (
            tr = taskResult(t1);
            if opts.Verbose or debugLevel > 0 then print ("dimViaBezout:  probabilistic dim finished first: " | toString(tr));
            cancel t2;
            return tr;            
        );
        if opts.Verbose or debugLevel > 0 then print "dimViaBezout: Something went wrong with multithreading.";              
        return null;
        *-
        
        curAttemptList := apply(attempts, i -> dimViaBezoutInternal(I1, DimensionIntersectionAttempts=>1, Replacement => replacementList, Homogeneous => homog, Verbose=>opts.Verbose));
        if opts.Verbose or debugLevel > 0 then print ("dimViaBezout: answers" | toString(curAttemptList));
        val = floor(0.25 + sum(curAttemptList)/attempts); --sort of a weighted rounding, since it seems we normally overestimate dim by this method
        --run it *attempts* times, then average
        return val;
    );
    if opts.Verbose or debugLevel > 0 then print "dimViaBezout: The field is too small, extending it.";
    if opts.Verbose or debugLevel > 0 then print ("dimViaBezout: New field size is " | toString(pp) | "^" | toString(i*d) | " = " | toString(pp^(i*d)) );
    (S2, phi1) := fieldBaseChange(S1, GF(pp, i*d));
    I2 := phi1(I1);    
    attemptList := apply(attempts, i -> dimViaBezoutInternal(I2, DimensionIntersectionAttempts=>1, Replacement => replacementList, Homogeneous => homog, Verbose=>opts.Verbose));
    if opts.Verbose or debugLevel > 0 then print ("dimViaBezout: answers" | toString(attemptList));
    val = floor(0.25 + sum(attemptList)/attempts); 
        --run it *attempts* times, then average
    return val;
)

--this function checks quickly hopefully, whether the following ideal saturates to nothing
fastCheckHomogIdealEmpty = method();

fastCheckHomogIdealEmpty := J1 -> (
    if (isDimAtMost(0, J1) === true) then return true;
    S1 := ring J1;
    return (dim(J1) <= 0);
    --return (saturateInGenericCoordinates(J1, Replacement => Monomial) == ideal 1_S1);    
);


dimViaBezoutInternal=method(Options => {Verbose => false, Replacement => null, Homogeneous => false, DimensionIntersectionAttempts => 1});

dimViaBezoutInternal(Ideal) := opts -> (I1)->(
    local myList;
    local i;
    local val;
    S1 := ring I1;
    --if (getFieldSize(S1)<39) then return codim I1;
    if (opts.Homogeneous) then i = dim S1 - 1 else i = dim S1;
    --i := dim S1-1;
    checks := opts.DimensionIntersectionAttempts;
    L1 := apply(checks, t->getRandomLinearForms(S1, opts.Replacement, Verify => true, Homogeneous=>opts.Homogeneous));
    
    while (i >= 0) do (
        if opts.Verbose or debugLevel > 0 then print("dimViaBezoutInternal: Trying intersection with a linear space of dimension " | toString(dim S1 - i) | ",");-- | toString(dim ideal (L1#0)));
        if (i == 0) then checks = 1;        
        --print L1;
        --print trim(I1 + L1);
        if (opts.Homogeneous) then (
            --myList = apply(L1, l -> ( saturateInGenericCoordinates((ideal l) + I1) != ideal 1_S1));
            myList = apply(L1, l -> (not fastCheckHomogIdealEmpty( (ideal l) + I1) ));
        )
        else (
            myList = apply(L1, l -> ((ideal l) + I1 != ideal 1_S1));
        );
        val = i;
        if (opts.Homogeneous) then val = val + 1;
        if all(myList, b->b) then return val;
        --if (L1 + I1 != ideal 1_S1) then return i;
        --print dim(L1 + I1);
        L1 = apply(L1, l -> drop(l, 1)); --drop something from each set
        i = i-1;
        --print i;
    );        
    return -1;
)




getFieldSize = method();

needsPackage "PushForward";

getFieldSize(Ring):= (k1) -> (    
    if instance(k1, GaloisField) then return (char k1)^((degree (ideal ambient k1)_0)#0);
    if instance(k1, QuotientRing) then (
        if ambient(k1) === ZZ then return char k1;
        pp := char k1;
        l1 := ZZ/pp[];
        inc := map(k1, l1, {});
        return pp^(rank ((pushFwd(inc))#0));
    );
    infinity
)

--The following is a Bezout function done via a binary search
-*
dimViaBezoutHomogeneous=method(Options => {DimensionIntersectionAttempts => 1});

dimViaBezoutHomogeneous(Ideal) := opts-> (I1) -> (
    S1 := ring I1;
    --if (getFieldSize(S1)<39) then return codim I1;
    i := dim S1-1;
    checks := opts.DimensionIntersectionAttempts;
    while (i >= 0) do (
        if opts.Verbose or debugLevel > 0 then print("dimViaBezoutNonhomogeneous: Trying intersection with a linear space of dimension " | toString(dim S1 - i));
        if (i == 0) then checks = 1;
        L1 := apply(checks, t->ideal getRandomLinearForms(S1, {0,0,0,0,i,0}, Homogeneous=>opts.Homogeneous));
        --print L1;
        --print trim(I1 + L1);
        myList := apply(L1, l -> (l + I1 != ideal 1_S1));
        if all(myList, b->b) then return i;
        --if (L1 + I1 != ideal 1_S1) then return i;
        --print dim(L1 + I1);
        i = i-1;
        --print i;
    );        
    return -1;
)
*-









randomPoints = method(TypicalValue => List, Options => optRandomPoints);

randomPoints(ZZ, Ideal) := List => opts -> (n1, I1) ->(
    randomPointsBranching(n1, I1, opts)
);
randomPoints(Ideal) := List => opts -> (I1) ->(
    randomPointsBranching(1, I1, opts)
);


randomPointsBranching = method(TypicalValue => List, Options => optRandomPoints);

randomPointsBranching(ZZ, Ideal) := List => opts -> (n1, I) -> (
    if (opts.Verbose) or (debugLevel > 0) then print concatenate("randomPointsBranching: starting with Strategy => ", toString(opts.Strategy));
    --if they give us an ideal of a quotient ring, then 
    if (class ring I === QuotientRing) 
    then return randomPointsBranching(n1, sub(I, ambient ring I) + ideal ring I, opts);
    
    --if it is not a quotient of polynomial ring, nor a polynomial ring, then we error
    if (not class ring I === PolynomialRing) 
    then error "randomPoints: must be an ideal in a polynomial ring or a quotient thereof";
    
    genList := first entries gens I;
    R := ring I;
    if (opts.Strategy == Default) then (
        return randomPointViaDefaultStrategy(n1, I, opts)
    )
    else if (opts.Strategy == BruteForce) 
    then (
        if (opts.NumThreadsToUse > 1) then return randomPointViaMultiThreads(n1, I, opts)
        else return searchPoints(n1, R, genList, opts);
    )	
	
    else if (opts.Strategy == LinearIntersection) 
    then return linearIntersectionNew(n1, I, opts)
    
    --else if (opts.Strategy == MultiplicationTable)
    --then return linearIntersectionNew(n1, I, opts++{DecompositionStrategy=>MultiplicationTable})
--    else if (opts.Strategy == LinearProjection) 
--    then return randomPointViaLinearProjection(I, opts)
    --else if (opts.Strategy == MultiThreads)
    --then return randomPointViaMultiThreads(I, opts)
    
    else error "randomPoints:  Not a valid Strategy";
);


randomPointViaMultiThreads = method(TypicalValue => List, Options => optRandomPoints);
randomPointViaMultiThreads(ZZ, Ideal) := List => opts -> (toFind, I) -> (
    if (debugLevel > 0) or (opts.Verbose) then print "randomPointViaMultiThreads: starting";
    genList := first entries gens I;
    R := ring I;
    K := coefficientRing R;
    n := #gens R;
    
    local found;
    local resultList;
    pointList := {};
    
    local numPointsToCheck;
    numPointsToCheck = floor(opts.PointCheckAttempts / opts.NumThreadsToUse); 
    
--    if opts.NumThreadsToUse > allowableThreads
--    then error "mtSearch: Not enough allowed threads to use";
    
    local flag;
    flag = new MutableList from apply(opts.NumThreadsToUse, i->0);
    
    taskList := apply(opts.NumThreadsToUse, (i)->(return createTask(mtSearchPoints, (toFind, numPointsToCheck, genList, {i,flag}, opts));));
    apply(taskList, t -> schedule t);
    while true do (
	    nanosleep 1000000; --one thousandth of a second
        if (all(taskList, t -> isReady(t))) then break;
    );
      
    resultList = apply(taskList, t -> taskResult(t));
    apply(#resultList, i -> pointList = pointList|(resultList#i));
 -*   
    if any(resultList, (l) -> (#l>0))
    then (
        j := 0;
        while #(resultList#j) == 0 do j = j + 1;
        return resultList#j;
    );
*-
    return pointList;
);

--some helper functions for randomPointViaMultiThreads

getAPoint = (n, K) -> (toList apply(n, (i) -> random(K)));

evalAtPoint = (kk, myMatrix, curPoint) -> (
    sourceRing := ring myMatrix;
    newPhi := map(kk, sourceRing, apply(curPoint, t -> sub(t, kk)));
    return newPhi(myMatrix)
);


evalAtPointIsZero = (R, genList, point) -> (
    K := coefficientRing R;
    n := #gens R;
    eval := map(K, R, point);
    for f in genList do ( 
    	if not eval(f) == 0 
	    then return false;
	);
    return true;
);

--a brute force function for multithreads
mtSearchPoints = method(Options => optRandomPoints);
mtSearchPoints(ZZ, ZZ, List, List) := opts -> (toFind, nn, genList, flagList) -> (
    if (debugLevel > 0) or (opts.Verbose) then (print "mtSearchPoints: starting thread #" | toString(nn));
    thNum := flagList#0;
    flag := flagList#1;
    local point;
    pointList := {};
    R := ring(genList#0);
    K := coefficientRing R;
    n := #gens R;
    i := 0;
    myMod := ceiling(nn/20);
    while (i < nn) do (
        if (i%(myMod) == 0) and (sum toList flag >= toFind) then break;
	    point = getAPoint(n, K);
	    --if verifyPoint(point, genList, opts)  --
        if evalAtPointIsZero(R, genList, point) and not (opts.Homogeneous and all(point, t -> t == 0))
	    then (
	        flag#thNum = flag#thNum+1;
            pointList = append(pointList, point);
	    );
        i = i+1;
	);
    return pointList;
);

--a brute force point search tool
searchPoints = method(Options => optRandomPoints);
searchPoints(ZZ, Ring, List) := opts -> (nn, R, genList) -> (
    local point;
    K := coefficientRing R;
    n := #gens R;
    pointList := {};
    i := 0;
    while (i < opts.PointCheckAttempts) and (#pointList < nn) do (
	    point = getAPoint(n, K);
	    if evalAtPointIsZero(R, genList, point) then (
            if not ((opts.Homogeneous) and (matrix{point} == 0)) then pointList = append(pointList, point);
        );
        i = i+1;
	);
    return pointList;
);



findANonZeroMinor = method(Options => optFindANonZeroMinor);

findANonZeroMinor(ZZ, Matrix, Ideal) := opts -> (n,M,I)->(
    P := {};
    local kk; 
    local kk2;
    local R;
    local phi;
    local N; local N1; local N2; local N1new; local N2new;
    local J; local Mcolumnextract; local Mrowextract;
    R = ring I;
    kk = coefficientRing R;
    i := 0;
    rk := -1;
    mutOptions := new MutableHashTable from opts;
    remove(mutOptions, MinorPointAttempts);
    ptOpts := new OptionTable from mutOptions;
    while (i < opts.MinorPointAttempts) and (rk < n) do (
        if opts.Verbose or debugLevel > 0 then print concatenate("findANonZeroMinor: Finding a point on the given ideal, attempt #", toString i);
        P = randomPoints(I, ptOpts);
        if #P > 0 then (
            P = P#0;
            kk2 = ring P#0;
            if opts.Verbose or debugLevel > 0 then print concatenate("findANonZeroMinor: Found a point over the ring ", toString(kk2));                        
            phi =  map(kk2,R,sub(matrix{P},kk2));    
            N = mutableMatrix phi(M);
            rk = rank(N);
            if opts.Verbose and  (rk < n) then print "findANonZeroMinor: The matrix didn't have the desired rank at this point.  We may try again";
        )
        else(
            if opts.Verbose or debugLevel > 0 then print concatenate("findANonZeroMinor: Failed to find a point, we may try again.");
        );
        i = i+1;
    );    
    if (rk < n) then error "findANonZeroMinor: All minors of given size vanish at the randomly chosen points. You may want to increase MinorPointAttempts, or change Strategy.";    
    if opts.Verbose or debugLevel > 0 then print "findANonZeroMinor:  The point had full rank.  Now finding the submatrix.";
    N1 = (columnRankProfile(N));
    Mcolumnextract = M_N1;
    M11 := mutableMatrix phi(Mcolumnextract);
    N2 = (rowRankProfile(M11));
    N1rand := random(N1);
    N1new = {};
    for i from  0 to n-1 do(
	    N1new = join(N1new, {N1rand#i});
    );
    M3 := mutableMatrix phi(M_N1new);
    --Karl:  I modified the following.
    if (rank(M3)<n) then error "findANonZeroMinor:  Something went wrong, the matrix rank fell taking the first submatrix.  This indicates a bug in the program.";
    --this is what was written before:
    --return (P,N1,N2,"findANonZeroMinor: Using the the second and third outputs failed to generate a random matrix of the given size, that has full rank when evaluated at the first output.");
    N2rand := random(rowRankProfile(M3));
    N2new = {};
    for i from 0 to n-1 do(
        N2new = join(N2new, {N2rand#i});
    );
    Mspecificrowextract := (M_N1new)^N2new;
    return (P, N1, N2, Mspecificrowextract);	
);

extendIdealByNonZeroMinor = method(Options=>optFindANonZeroMinor);
extendIdealByNonZeroMinor(ZZ,Matrix,Ideal):= opts -> (n, M, I) -> (
    local O;  
    local Ifin;
    O = findANonZeroMinor(n,M,I,opts); 
    L1 := ideal (det(O#3));
    Ifin = I + L1;
    return Ifin;
);

--**************************************
--the following is taken from FastMinors
--**************************************

isGBDone := (myGB) -> (
    --a temporary function for finding out if a gb computation is done.
    myStr := status myGB;
    return 0 < #select("status: done", myStr);
);
isCodimAtLeast = method(Options => {
    Verbose => false,
    PairLimit => 200
    --DegreeFunction => ( (t,i) -> ceiling((i+1)*t))
});

isCodimAtLeast(ZZ, Ideal) := opts -> (n1, I1) -> (
    R1 := ring I1;
    S1 := ambient R1;
    if (not isPolynomialRing(S1)) then error "isCodimAtLeast:  This requires an ideal in a polynomial ring, or in a quotient of a polynomial ring.";
    if (n1 <= 0) then return true; --if for some reason we are checking codim 0.
    if (isMonomialIdeal(I1)) then (
        if (codim monomialIdeal(I1) >= n1) then return true;
    );
    if (#first entries gens I1 == 0) then ( return false; ); 
    J1 := ideal R1;
    dAmb := codim J1;
    I2 := sub(I1, S1) + sub(J1, S1); --lift to the polynomial ring.
    --now we have an ideal in a polynomial ring.  The idea is that we should compute a partial Groebner basis.
    --and compute the codim of that.
    --But first, we just try a quick codim computation based on the ideal generators.
    monIdeal := null;
    if (#first entries gens I2 > 0) then (
        monIdeal = monomialIdeal(apply(first entries gens I2, t->leadTerm t));
        if (opts.Verbose or debugLevel > 2) then print concatenate("isCodimAtLeast: Computing codim of monomials based on ideal generators.");
        if (codim monIdeal - dAmb >= n1) then return true;
        if (opts.Verbose or debugLevel > 2) then print concatenate("isCodimAtLeast: Didn't work, going to find the partial Groebner basis.");
    );
    --now we set stuff up for the loop if that doesn't work.  
    vCount := # first entries vars S1;
 --   baseDeg := apply(sum(apply(first entries vars S1, t1 -> degree t1)), v -> ceiling(v/vCount)); --use this as the base degree to step by (probably we should use a different value)
    i := 1;
    --curLimit := baseDeg;
    local curLimit;
    local myGB;
    
    gensList := null;
    while (i < opts.PairLimit) do(
        curLimit = ceiling(i^1.5);
--        curLimit = apply(baseDeg, tt -> (opts.SPairsFunction)(tt,i));
        if (opts.Verbose or debugLevel > 2) then print concatenate("isCodimAtLeast: about to compute gb PairLimit => ", toString curLimit);
        myGB = gb(I2, PairLimit=>curLimit);
        gensList = first entries leadTerm gb(I2, PairLimit=>curLimit);    
        if (#gensList > 0) then (
            monIdeal = monomialIdeal(first entries leadTerm myGB);
            if (opts.Verbose or debugLevel > 2) then print concatenate("isCodimAtLeast: computed gb, now computing codim ");
            if (codim monIdeal - dAmb >= n1) then return true;
        );
        if (isGBDone(myGB)) then i = opts.PairLimit;
        i = i + 1;
    );
    return null;
);

isDimAtMost = method(Options => {
    Verbose => false,
    PairLimit => 200
    --DegreeFunction => ( (t,i) -> ceiling((i+1)*t))    
});

isDimAtMost(ZZ, Ideal) := opts -> (n1, I1) -> (
    d := dim ring I1;
    return isCodimAtLeast(d-n1, I1, opts);
);



---


-- A function with an optional argument


beginDocumentation()
document {
        Key => RandomPoints,
        Headline => "Obtain random points in a variety",
        EM "RandomPoints", "Find random points inside a variety.",
        BR{},BR{},
        "This package provides tools for quickly finding a point (rational, or over a field extension) in the vanishing set of an ideal.  The search is highly customizable.  This package also includes tools for finding submatrices of a given rank at some point.  Furthermore, it provides tools for generic projections and producing collections of linear forms with specified properties.",
        BR{},
        BOLD "Core functions:",
        UL {
            {TO "randomPoints", ":  This tries to find a point in the vanishing set of an ideal."},
            {TO "findANonZeroMinor", ":  This finds a submatrix of a given matrix that is nonsingular at a point of a given ideal."},            
            {TO "extendIdealByNonZeroMinor", ":  This extends an ideal by a minor produced by ", TO "findANonZeroMinor", "."},
            {TO "projectionToHypersurface", " and ", TO "genericProjection", ":  These functions provide customizable projection."}
	    },
        BR{},BR{},
	    BOLD "Acknowledgements:",BR{},BR{},
	    "The authors would like to thank David Eisenbud and Mike Stillman for useful conversations and comments on the development of this package.  The authors began work on this package at the virtual Cleveland 2020 Macaulay2 workshop."
}



doc ///
    Key
        getRandomLinearForms
        (getRandomLinearForms, Ring, List)
        [getRandomLinearForms, Verify]
        [getRandomLinearForms, Homogeneous]
        [getRandomLinearForms, Verbose]
    Headline
        retrieve a list of random degree 1 and 0 forms of specified types
    Usage
        getRandomLinearForms(R, L)
    Inputs
        R:Ring
            the ring where the forms should live
        L:List
            a list with 6 entries, each a number of types of forms.  Constant forms, monomial forms (plus a constant term if {\tt Homogeneous => false}), monomial forms, binomial forms, trinomial forms, and random forms.
        Verify => Boolean
            whether to check if the output linear forms have Jacobian of maximal rank
        Verbose => Boolean
            turn on or off verbose output
        Homogeneous => Boolean
            allows constant terms on some linear forms if true
    Outputs
        :List
            a list of random forms of the specified types
    Description
        Text
            This will give you a list of random forms (ring elements) of the specified types.  This is useful, because in many cases, for instance when doing generic projection, you only need a a certain number of the forms in the map to be fully random.  Furthermore, at the cost of some randomness, using monomial or binomial forms can be much faster.            

            The types of form are specified via the second argument, a list with 5 entries.  The first entry is how many constant forms are allowed.
        Example
            R = ZZ/31[a,b,c]
            getRandomLinearForms(R, {2,0,0,0,0,0})
        Text
            The second entry in the list is how many monomial forms are returned.  Note if {\tt Homogeneous=>false} then these forms will usually have constant terms.
        Example
            getRandomLinearForms(R, {0,2,0,0,0,0}, Homogeneous=>true)
            getRandomLinearForms(R, {0,2,0,0,0,0}, Homogeneous=>false)
        Text
            Next, the third entry is how many monomial forms (without constant terms, even if {\tt Homogeneous=>false}).
        Example
            getRandomLinearForms(R, {0,0,2,0,0,0}, Homogeneous=>false)
        Text
            The fourth entry is how many binomial forms should be returned.
        Example
            getRandomLinearForms(R, {0,0,0,1,0,0}, Homogeneous=>true)
            getRandomLinearForms(R, {0,0,0,1,0,0}, Homogeneous=>false)
        Text
            The ultimate entry is how many truly random forms to produce.
        Example
            getRandomLinearForms(R, {0,0,0,0,0,1}, Homogeneous=>true)
            getRandomLinearForms(R, {0,0,0,0,0,1}, Homogeneous=>false)
        Text
            You may combine the different specifications to create a list of the desired type.  The order is randomized.

            If the option {\tt Verify=>true}, then this will check the jacobian of the list of forms (discounting the constant forms), to make sure it has maximal rank.  Random forms in small numbers of variables over small fields will produce non-injective ring maps occasionally otherwise.        
///


doc ///
    Key
        ExtendField
        [randomPoints, ExtendField]
        [findANonZeroMinor, ExtendField]      
        [extendIdealByNonZeroMinor, ExtendField]  
    Headline
        an option used to specify if extending the finite field is permissible here
    Usage
        ExtendField => b
    Inputs
        b:Boolean
            whether the base field is allowed to be extended
    Description
        Text
            Various functions which produce points, or call functions which produce points, may naturally find scheme theoretic points that are not rational over the base field (for example, by intersecting with a random linear space).  Setting {\tt ExtendField => true} will tell the function that such points are valid.  Setting {\tt ExtendField => false} will tell the function ignore such points.  This sometimes can slow computation, and other times can speed it up.  In some cases, points over extended fields may also have better randomness properties for applications.
    SeeAlso
        randomPoints
        findANonZeroMinor
        extendIdealByNonZeroMinor
///

doc ///
    Key
        genericProjection
        (genericProjection, Ideal)
        (genericProjection, Ring)
        (genericProjection, ZZ, Ideal)
        (genericProjection, ZZ, Ring)
        [genericProjection, Homogeneous]
    Headline
       finds a random (somewhat) generic projection of the ring or ideal
    Usage
        genericProjection(n, I)
        genericProjection(n, R)
        genericProjection(I)
        genericProjection(R)
    Inputs
        I:Ideal 
            in a polynomial ring
        R:Ring
            a quotient of a polynomial ring
        n:ZZ
            an integer specifying how many dimensions to drop
        MaxCoordinatesToReplace => ZZ
            to be passed to randomCoordinateChange
        Replacement => Symbol
            to be passed to randomCoordinateChange
        Homogeneous => Boolean
            to be passed to randomCoordinateChange
    Outputs
        :List
            a list with two entries, the generic projection map, and the ideal if I was provided, or the ring if R was provided
    Description
        Text
            This gives the projection map from $\mathbb{A}^N \mapsto\mathbb{A}^{N-n}$ and the defining ideal of the projection of $V(I)$
        Example
            R=ZZ/5[x,y,z,w];
            I = ideal(x,y^2,w^3+x^2);
            genericProjection(2,I)
        Text
            If no integer $n$ is provided, then drops one dimension, in other words it treats $n = 1$.
        Example
            R=ZZ/5[x,y,z,w];
            I = ideal(x,y^2);
            genericProjection(I)
        Text
            Alternately, instead of {\tt I}, you may pass it a quotient ring.  It will then return the inclusion of the generic projection ring into the given ring, followed by the source of that inclusion.  It is essentially the same functionality as calling {\tt genericProjection(n, ideal R)} although the format of the output is slightly different.
        Example
            R = ZZ/13[x,y,z];
            I = ideal(y^2*z-x*(x-z)*(x+z));
            genericProjection(R/I)
        Text
            This method works by calling {\tt randomCoordinateChange} before dropping some variables.  It passes the options {\tt Replacement}, {\tt MaxCoordinatesToReplace}, {\tt Homogeneous} to that function.
        Text
            This function makes no attempt to verify that the projection is actually generic with respect to the ideal.  
    SeeAlso
        randomCoordinateChange
///

doc ///
    Key
        randomCoordinateChange
        (randomCoordinateChange, Ring)
        [randomCoordinateChange, Homogeneous]
    Headline
        produce linear automorphism of the ring
    Usage
        randomCoordinateChange R
    Inputs
        R:Ring
            a polynomial Ring
        MaxCoordinatesToReplace => ZZ 
            how many coordinates should be replaced by linear functions
        Replacement => Symbol 
            whether coordinate replacements should be binomial (Binomial) or fully random (Full) 
        Homogeneous => Boolean
            whether coordinate replacements should be Homogeneous
        Verbose => Boolean
            set to true for verbose output
    Outputs
        :RingMap
            the coordinate change map.
    Description
        Text
            Given a polynomial ring, this will produce a linear automorphism of the ring. 
        Example
            R=ZZ/5[x,y,z]
            randomCoordinateChange(R)
        Text
            In some applications, a full change of coordinates is not desired, as it might cause code to run slowly, and so a Binomialr change of coordinates might be preferred.  
            These Binomial changes of coordinates can be accomplished with the options {\tt Replacement} and {\tt MaxCoordinatesToReplace}.
            {\tt Replacement} can take either {\tt Binomial} or {\tt Full}.  If {\tt Binomial} is specified, then only binomial changes of coordinates will be produced. 
        Example
            S = ZZ/11[a..e]
            randomCoordinateChange(S, Replacement=>Binomial)
        Text
            Finally, if {\tt Homogeneous} is set to {\tt false}, then our change of coordinates is not homogeneous (although it is still linear).
        Example 
            randomCoordinateChange(R, Homogeneous=>false)
        Text
            Note, this function already checks whether the function is an isomorphism by computing the Jacobian.
///

doc ///
    Key
        [genericProjection, Verbose]
        [randomCoordinateChange, Verbose]
        [randomPoints, Verbose]
        [projectionToHypersurface, Verbose]
    Headline
        turns out Verbose (debugging) output
    Description
        Text
            Set the option {\tt Verbose => true} to turn on verbose output.  This may be useful in debugging or in determining why an computation is running slowly. 
///

doc ///
    Key
        projectionToHypersurface
        (projectionToHypersurface, Ideal)
        (projectionToHypersurface, Ring)
        [projectionToHypersurface, Codimension]
        [projectionToHypersurface, Homogeneous]
        Codimension
    Headline
        Generic projection to a hypersurface
    Usage
        projectionToHypersurface I
        projectionToHypersurface R 
    Inputs
        I:Ideal
            an ideal in a polynomial ring
        R:Ring
            a quotient of a polynomial ring
        Codimension => ZZ
            specified if you already know the codimension of your Ideal (or QuotientRing) in your ambient ring
        MaxCoordinatesToReplace => ZZ
            to be passed to randomCoordinateChange
        Replacement => Symbol
            to be passed to randomCoordinateChange
        Homogeneous => Boolean
            to be passed to randomCoordinateChange
        Verbose => Boolean
            set to true for verbose output
    Outputs
        :RingMap
            a list with two entries, the generic projection map, and the ideal if {\tt I} was provided, or the ring if {\tt R} was provided
    Description
        Text
            This creates a projection to a hypersurface.  It differs from {\tt genericProjection(codim I - 1, I)} as it only tries to find a hypersurface equation that vanishes along the projection, instead of finding one that vanishes exactly at the projection.  This can be faster, and can be useful for finding points.
        Example
            R=ZZ/5[x,y,z];
            I = ideal(random(3,R)-2, random(2,R));
            projectionToHypersurface(I)
            projectionToHypersurface(R/I)
        Text
            If you already know the codimension is {\tt c}, you can set {\tt Codimension=>c} so the function does not compute it.
    SeeAlso
        genericProjection
///

doc///
    Key
        MaxCoordinatesToReplace
        [randomCoordinateChange, MaxCoordinatesToReplace]        
        [genericProjection, MaxCoordinatesToReplace]        
        [projectionToHypersurface, MaxCoordinatesToReplace]        
    Headline
        The maximum number of coordinates to turn into non-monomial functions when calling {\tt randomCoordinateChange}
    Description
        Text
            When calling {\tt randomCoordinateChange}, the user can specify that only a specified number of coordinates should be non-monomial.  Sometimes, generic coordinate changes where all coordinates are modified, can be very slow.  This is a way to mitigate for that.
            This option is passed to {\tt randomCoordinateChange} by other functions that call it.
        Example
            S = ZZ/11[a..e]
            randomCoordinateChange(S, MaxCoordinatesToReplace=>2)
    SeeAlso
        randomCoordinateChange
///

doc ///
    Key
        Replacement
        [randomCoordinateChange, Replacement]
        [genericProjection, Replacement]
        [projectionToHypersurface, Replacement]
        [findANonZeroMinor, Replacement]
        [randomPoints, Replacement]
        [extendIdealByNonZeroMinor, Replacement]
        [dimViaBezout, Replacement]
        Full
        Trinomial
    Headline
        When changing coordinates, whether to replace variables by general degre 1 forms, binomials, etc.
    Usage
        Replacement => Full
        Replacement => Monomial
        Replacement => Binomial
        Replacement => Trinomial        
    Description
        Text
            When calling various functions, setting {\tt Replacement => Full} will mean that coordinates are changed to a general degree 1 form.  If {\tt Replacement => Binomial}, the coordinates are only changed to binomials, which can be much faster for certain applications.  Other options include {\tt Replacement => Monomial} and {\tt Replacement => Trinomial}.
        Example
            R = ZZ/101[a,b,c,d,e];            
            randomCoordinateChange(R, Replacement=>Monomial)
            randomCoordinateChange(R, Replacement=>Binomial)
            randomCoordinateChange(R, Replacement=>Trinomial)
            randomCoordinateChange(R, Replacement=>Full)
        Text
            If {\tt Homogeneous => false} in these cases, then there will be constant terms, and we view $mx + b$ as a monomial.
        Example
            S = ZZ/103[x,y,z,u,v];            
            randomCoordinateChange(S, Replacement => Monomial, Homogeneous => false)
            randomCoordinateChange(S, Replacement => Binomial, Homogeneous => false)
            randomCoordinateChange(S, Replacement => Trinomial, Homogeneous => false)
            randomCoordinateChange(S, Replacement => Full, Homogeneous => false)
    SeeAlso
        randomCoordinateChange
///

doc ///
    Key
        [randomPoints, Strategy]
        [findANonZeroMinor, Strategy]
        [extendIdealByNonZeroMinor, Strategy]
        Default
        BruteForce        
        LinearIntersection    
        MultiplicationTable    
    Headline
        values for the option Strategy when calling randomPoints
    Description
        Text
            When calling {\tt randomPoints}, set the strategy to one of these.
            {\tt BruteForce} simply tries random points and sees if they are on the variety.
	    
            {\tt LinearIntersection} intersects with an random linear space.  Setting the {\tt DecompositionStrategy => MultiplicationTable} or {\tt DecompositionStrategy=>Decompose} will change how ideals corresponding to points are broken up into minimal primes which can have a substantial impact on speed.  Otherwise, the function chooses which strategy it thinks will be better.  See @TO DecompositionStrategy@.  

            {\tt Default} performs a sequence of different strategies, with successively increasing complexity of the linear subspaces that are intersected.
    SeeAlso
        randomPoints
        randomKRationalPoint
        projectionToHypersurface
///

doc///
    Key 
        NumThreadsToUse
        [randomPoints, NumThreadsToUse]
        [extendIdealByNonZeroMinor, NumThreadsToUse]
        [findANonZeroMinor, NumThreadsToUse]
    Headline
        number of threads the the function will use in a brute force search for a point 
    Description
        Text
            When calling {\tt randomPoints}, and functions that call it, with a {\tt BruteForce} strategy, this denotes the number of threads to use in brute force point checking.
    Caveat
        Currently multi threading creates instability.  Use at your own risk.
    SeeAlso
        randomPoints
///

doc///
    Key 
        PointCheckAttempts
        [randomPoints, PointCheckAttempts]
        [extendIdealByNonZeroMinor,PointCheckAttempts ]
        [findANonZeroMinor, PointCheckAttempts]
    Headline
        Number of times the the function will search for a point 
    Description
        Text
            When calling {\tt randomPoints}, and functions that call it, with a {\tt BruteForce} strategy strategy, this denotes the number of trials for brute force point checking.  When calling it with a {\tt LinearIntersection} strategy, this controls how many linear spaces are created.
        Example
            R = ZZ/11[x,y,z];
            I = ideal(x,y);
            randomPoints(I, PointCheckAttempts=>1)
            randomPoints(I, PointCheckAttempts=>1000)
    SeeAlso
        randomPoints
        extendIdealByNonZeroMinor
        findANonZeroMinor
///

doc ///
    Key
        DecompositionStrategy
        [randomPoints, DecompositionStrategy]
        [extendIdealByNonZeroMinor,DecompositionStrategy]
        [findANonZeroMinor, DecompositionStrategy]
    Headline
        control how ideals of points are factored into minimal primes
    Description
        Text
            In many cases using a multiplication table (ie, computing how a variable acts on residue fields of points in two different ways) can be used to more quickly decompose ideals.  This is turned on by setting {\tt DecompositionStrategy => MultiplicationTable}.  However, in other cases, especially when there are many variables, using {\tt DecompositionStrategy => Decompose} can be substantially faster.  Currently {\tt MultiplicationTable} only works with homogeneous ideals and will not find geometric points.
///

doc ///
    Key
        randomPoints
        (randomPoints, Ideal)
        (randomPoints, ZZ, Ideal)
        [randomPoints, Homogeneous]                
        [randomPoints, DimensionFunction]        
    Headline
        a function to find random points  in a variety. 
    Usage
        randomPoints(I) 
        randomPoints(n, I)        
    Inputs
        n: ZZ
            an integer denoting the number of desired points.
        I:Ideal
            inside a polynomial ring.
        R:Ring
            a polynomial ring
        Strategy => Symbol
            to specify which strategy to use, Default, BruteForce, LinearIntersection.        
        ExtendField => Boolean
            whether to allow points not rational over the base field        
        Homogeneous => Boolean
            whether to include the origin as a valid point to output
	    PointCheckAttempts => ZZ
	        points to search in total, see @TO PointCheckAttempts@
        NumThreadsToUse => ZZ
	        number of threads to use in the BruteForce strategy, see @TO NumThreadsToUse@
        DimensionFunction => Function
            specify a custom dimension function, such as the default dimViaBezout or the Macaulay2 function dim
        DecompositionStrategy => Symbol
            see @TO DecompositionStrategy@
    Outputs
        :List
            a list of points in the variety with possible repetitions.
    Description
        Text  
           Gives at most $n$ many point in a variety $V(I)$. 
        Example
            R = ZZ/5[t_1..t_3];
            I = ideal(t_1,t_2+t_3);
            randomPoints(3, I)
            randomPoints(4, I, Strategy => Default)            
            randomPoints(4, I, Strategy => LinearIntersection)            
        Text
            The default strategy switches between LinearIntersection and MultiplicationTable for the @TO DecompositionStrategy@ (after first trying a brute force strategy).
        Text
            Using {\tt DecompositionStrategy => MultiplicationTable} (currently only implemented for Homogeneous ideals) is sometimes faster and other times not.  It tends to work better in rings with few variables.  Because of this, the default strategy runs {\tt MultiplicationTable} first in rings with fewer variables and runs {\tt Decompose} first in rings with more variables.
        Example
            S=ZZ/103[y_0..y_30];
            I=minors(2,random(S^3,S^{3:-1}));
            elapsedTime randomPoints(I, Strategy=>LinearIntersection, DecompositionStrategy=>MultiplicationTable)
            elapsedTime randomPoints(I, Strategy=>LinearIntersection, DecompositionStrategy=>Decompose)        
///

doc ///
    Key
        dimViaBezout
        (dimViaBezout, Ideal)
        [dimViaBezout, DimensionIntersectionAttempts]
        [dimViaBezout, MinimumFieldSize]
        [dimViaBezout, Verbose]
        [dimViaBezout, Homogeneous]        
        MinimumFieldSize
        DimensionIntersectionAttempts        
    Headline
        computes the dimension of the given ideal $I$ probabilistically
    Usage
        dimViaBezout(I)
    Inputs
        I: Ideal
            in a polynomial ring over a field
        DimensionIntersectionAttempts => ZZ
            the number of linear spaces to try before moving to the next dimension
        MinimumFieldSize => ZZ
            if the ambient field is smaller than this value it will automatically be replaced with an extension
    Outputs
        : ZZ
            d = dimension of the ideal $I$
    Description
        Text
            This intersects $V(I)$ with successively higher dimensional random linear spaces until there is an intersection.  For example, if $V(I)$ intersect a random line has a point, then we expect that $V(I)$ contains a hypersurface.  If there was no intersection, this function tries a 2-dimensional linear space, and so on.  This greatly speeds up some computations, although in other examples, the built in {\tt dim} function is much faster.
        Example
            kk=ZZ/101;
            S=kk[y_0..y_8];
            I=ideal random(S^1,S^{-2,-2,-2,-2})+(ideal random(2,S))^2;
            elapsedTime dimViaBezout(I)
            elapsedTime dim I
        Text
            The user may set the {\tt MinimumFieldSize} to ensure that the field being worked over is big enough.  For instance, there are relatively few linear spaces over a field of characteristic 2, and this can cause incorrect results to be provided.  If no size is provided, the function tries to guess a good size based on ambient ring.
        Text
            If the option {\tt Homogeneous=>true} then we use homogeneous linear spaces if the ideal itself is homogeneous.  Otherwise our linear spaces are not homogeneous.
        Text
            The user may also specify what sort of linear forms to intersect with via the @TO Replacement@ option.
    SeeAlso
        DimensionFunction
///

doc ///
    Key
        DimensionFunction
    Headline
        an option for specifying custom dimension functions
    Usage
        DimensionFunction => myFunction
    Description
        Text
            This package provides a custom dimension function for probabilistically computing the dimension, {\tt dimViaBezout}.  However, in some cases this can be substantially slower than calling the built in function {\tt dim}.  Thus the user may switch to using the built in function, or their own custom dimension function, via the option {\tt DimensionFunction => ...}.
    SeeAlso
        dim
        dimViaBezout
///

doc ///
    Key
        findANonZeroMinor
        (findANonZeroMinor, ZZ, Matrix, Ideal)
        [findANonZeroMinor, Verbose]
        [findANonZeroMinor, Homogeneous]        
        [findANonZeroMinor, MinorPointAttempts]
        [findANonZeroMinor, DimensionFunction]
        MinorPointAttempts
    Headline
        finds a non-vanishing minor at some randomly chosen point 
    Usage
        findANonZeroMinor(n,M,I)        
    Inputs
        I: Ideal
            in a polynomial ring over QQ or ZZ/p for p prime 
        M: Matrix
            over the polynomial ring
        n: ZZ
            the size of the minors to consider
        Strategy => Symbol
            to specify which strategy to use when calling @TO randomPoints@
        Verbose => Boolean
            set to true for verbose output
        Homogeneous => Boolean
            controls if the computations are homogeneous (in calls to {\tt randomPoints})
        MinorPointAttempts => ZZ
            how many points to check the rank of the matrix at
        DimensionFunction => Function
            specify a custom dimension function, such as the default dimViaBezout or the Macaulay2 function dim       
    Outputs
        : Sequence
            The functions outputs the following:
            
            1. randomly chosen point $P$ in $V(I)$, 
            
            2. the indexes of the columns of $M$ that stay linearly independent upon plugging $P$ into $M$, 

            3. the indices of the linearly independent rows of the matrix extracted from $M$ using (2), 

            4. a random $n\times n$ submatrix of $M$ that has full rank at $P$.
    Description
        Text
            Given an ideal, a matrix, an integer and a user defined Strategy, this function uses the 
            {\tt randomPoints} function to find a point in 
            $V(I)$. Then it plugs the point in the matrix and tries to find
            a non-zero  minor of size equal to the given integer. It outputs the point and also one of the submatrices of interest
            along with the column and row indices that were used sequentially.              
        Example
            R = ZZ/5[x,y,z];
            I = ideal(random(3,R)-2, random(2,R));
            M = jacobian(I);
            findANonZeroMinor(2,M,I)
        Text
            The option {\tt MinorPointAttempts} is how many points to attempt before giving up.
    SeeAlso
        randomPoints
///


doc ///
    Key
        extendIdealByNonZeroMinor
        (extendIdealByNonZeroMinor, ZZ, Matrix, Ideal)
        [extendIdealByNonZeroMinor, Homogeneous]        
        [extendIdealByNonZeroMinor, MinorPointAttempts]        
        [extendIdealByNonZeroMinor, Verbose]        
        [extendIdealByNonZeroMinor, DimensionFunction]        
    Headline
        extends the ideal to aid finding singular locus
    Usage
        extendIdealByNonZeroMinor(n,M,I)        
    Inputs
        I: Ideal
            in a polynomial ring over QQ or ZZ/p for p prime 
        M: Matrix
            over the polynomial ring
        n: ZZ
            the size of the minors to consider            
        Strategy => Symbol
            specify which strategy to use when calling @TO randomPoints@
        Homogeneous => Boolean
            controls if the computations are homogeneous (in calls to {\tt randomPoints})
        Verbose => Boolean
            turns on or off verbose output
        MinorPointAttempts => ZZ
            how many points to check the rank of the matrix at     
        DimensionFunction => Function
            specify a custom dimension function, such as the default dimViaBezout or the Macaulay2 function dim       
    Outputs
        : Ideal
            the original ideal extended by the determinant of 
            the non vanishing minor found
    Description
        Text
            This function finds a submatrix of size $n\times n$ using {\tt findANonZeroMinor};  
            it extracts the last entry of the output, finds its determinant and
            adds it to the ideal $I$, thus extending $I$.
        Example
            R = ZZ/5[x,y,z];
            I = ideal(random(3,R)-2, random(2,R));
            M = jacobian(I);
            extendIdealByNonZeroMinor(2,M,I, Strategy => LinearIntersection)
        Text
            One use for this function can be in showing a certain rings are R1 (regular in codimension 1).  Consider the following example which is R1 where computing the dimension of the singular locus takes around 30 seconds as there are 15500 minors of size $4 \times 4$ in the associated $7 \times 12$ Jacobian matrix.  However, we can use this function to quickly find interesting minors.  
        Example
            T = ZZ/101[x1,x2,x3,x4,x5,x6,x7];
            I =  ideal(x5*x6-x4*x7,x1*x6-x2*x7,x5^2-x1*x7,x4*x5-x2*x7,x4^2-x2*x6,x1*x4-x2*x5,x2*x3^3*x5+3*x2*x3^2*x7+8*x2^2*x5+3*x3*x4*x7-8*x4*x7+x6*x7,x1*x3^3*x5+3*x1*x3^2*x7+8*x1*x2*x5+3*x3*x5*x7-8*x5*x7+x7^2,x2*x3^3*x4+3*x2*x3^2*x6+8*x2^2*x4+3*x3*x4*x6-8*x4*x6+x6^2,x2^2*x3^3+3*x2*x3^2*x4+8*x2^3+3*x2*x3*x6-8*x2*x6+x4*x6,x1*x2*x3^3+3*x2*x3^2*x5+8*x1*x2^2+3*x2*x3*x7-8*x2*x7+x4*x7,x1^2*x3^3+3*x1*x3^2*x5+8*x1^2*x2+3*x1*x3*x7-8*x1*x7+x5*x7);
            M = jacobian I;
            i = 0;
            J = I;
            elapsedTime(while (i < 10) and dim J > 1 do (i = i+1; J = extendIdealByNonZeroMinor(4, M, J)) );
            dim J
            i
        Text
            In this particular example, there tend to be about 5 associated primes when adding the first minor to J, and so one would expect about 5 steps as each minor computed most likely will eliminate one of those primes.
        Text
            There is some similar functionality obtained via heuristics (as opposed to actually finding rational points) in the package "FastMinors".
    SeeAlso
        findANonZeroMinor
///

 ----- TESTS -----

--this test tests ....
TEST/// 
R=ZZ/5[x,y,z,w];
I = ideal(x,y^2,w^3+x^2);
genericProjection(2,I);
--assert(map)
///

--testing randomCoordinateChange with Homogeneous => true
TEST///
R = QQ[x,y,z,w];
phi = randomCoordinateChange(R, Homogeneous=>true);
m = ideal(x,y,z,w);
S = source phi;
n = sub(m, S);
assert(preimage(phi, m) == n);  --if we are homogeneous, this should be true
assert(phi(n) == m);
///

--testing randomCoordinateChange with Homogeneous => false
TEST ///
R = ZZ/1031[x,y,z,u,v,w];
phi = randomCoordinateChange(R, Homogeneous => false);
m = ideal(x,y,z);
S = source phi;
n = sub(m, S);
assert(dim phi(n) == dim m);
assert(dim preimage(phi, m) == dim n);
assert(preimage(phi, m) != n); --there is a theoretical chance this could happen, about 1 in 10^18.
assert(phi(n) != m);
///

--testing randomCoordinateChange with MaxCoordinatesToReplace => 0
TEST ///
R = ZZ/11[x,y,z];
phi = randomCoordinateChange(R, MaxCoordinatesToReplace => 0);
M = matrix phi;
S1 = set first entries M;
S2 = set gens R;
assert(isSubset(S1, S2) and isSubset(S2, S1));
///

--verifying Binomial vs Full replacement
TEST ///
R = ZZ/1031[a..h];
phi = randomCoordinateChange(R, Replacement=>Binomial);
psi = randomCoordinateChange(R, Replacement=>Full);
assert(all(apply(first entries matrix phi, v -> terms v), t -> #t <= 2));
assert(any(apply(first entries matrix psi, v -> terms v), t -> #t >= 3)); --this could be false, and an asteroid could destroy Earth.
///

--testing genericProjection, on an ideal
TEST///
R = ZZ/101[a,b,c];
I = ideal(a,b,c);
L = genericProjection(2, I, Homogeneous => true);
assert(dim source (L#0) == 1);
assert(dim (L#1) == 0);  
L2 = genericProjection(I, Homogeneous => true);
assert(dim source (L2#0) == 2);
assert(dim (L2#1) == 0);  
L3 = genericProjection(2, I, Homogeneous => false)
assert(dim (L3#1) == 0)
///

--testing genericProjection, on a ring
TEST///
R = ZZ/101[x,y,z]/ideal(y^2*z-x*(x-z)*(x+z));
L = genericProjection(1, R, Homogeneous => true); --we are already a hypersurface, so this should turn into a polynomial ring
assert(dim source (L#0) == 2);
assert(ker (L#0) == 0)
L2 = genericProjection(1, R, Homogeneous => false);
assert(dim source (L#0) == 2);
assert(ker (L2#0) == 0)
///

--testing projectionToHypersurface, for an ideal and a ring
TEST///
R = ZZ/11[x,y,z];
I = ideal(random(2, R), random(3, R));
L = projectionToHypersurface(I);
assert(dim source(L#0) == 2); --we should drop one dimension
assert(codim(L#1) == 1);
L2 = projectionToHypersurface(R/I);
assert(dim source(L2#0) == 1)
assert(codim(L2#1) == 1)
///

TEST///
---this tests findANonZeroMinor---
R = ZZ/5[x,y,z];
I = ideal(random(3,R)-2, random(2,R));
M = jacobian(I);
Output = findANonZeroMinor(2,M,I);
phi = map(ZZ/5, R, sub(matrix{Output#0},ZZ/5));
assert(det(phi(Output#3))!=0)
///


TEST///
---this tests extendIdealByNonZeroMinor---
R = ZZ/7[t_1..t_3];
I = ideal(t_1,t_2+t_3);
M = jacobian I;           
assert(dim extendIdealByNonZeroMinor(2,M,I,Strategy => LinearIntersection) < 1)
///

TEST///
---this tests whether extending the field works
R = ZZ/5[x];
I = ideal(x^2+x+1); --irreducible
assert(#randomPoints(1, I) == 0);
assert(#randomPoints(1, I, Strategy=>BruteForce) == 0);
assert(#randomPoints(1, I, ExtendField=>true) > 0);
///

TEST ///
S=ZZ/101[y_0..y_19];
I=ideal random(S^1,S^{5:-1});
assert(dimViaBezout I == dim I);

S=ZZ/103[y_0..y_20];
I=ideal random(S^1,S^{8:-1});
assert(dimViaBezout I ==dim I)
///

end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.

installPackage "RandomPoints"
installPackage("RandomPoints", RemakeAllDocumentation=>true)
check RandomPoints

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=RandomPoints pre-install"
-- End:

--
--Version notes
--**Version 1.5
------Renamed to RandomPoints
------Added dimViaBezout
------Improved speed of finding points
------Removed options related to generic projection (since its performance is relatively lower now)
------Other minor speed increases
--**Version 1.5.1
------Improved randomRationalPoint to identify no rational points on zero dimension schemes more quickly.
