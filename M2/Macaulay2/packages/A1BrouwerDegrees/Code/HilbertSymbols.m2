

-- Over k = R, the real symbol is 1 if either a or b is positive, and -1 if they are both negative.
-- See Serre, III Theorem 1

HilbertSymbolReal = method()
HilbertSymbolReal (QQ, QQ) := (ZZ) => (a, b) -> (
    if (a==0) then (error "first argument to HilbertSymbolReal must be nonzero");
    if (b==0) then (error "second argument to HilbertSymbolReal must be nonzero");
    if (a < 0 and b < 0) then (
	return -1;
	)
    else (
	if (a>0 or b>0) then (
	    return 1;
	    );
	);
    );

HilbertSymbolReal (QQ, ZZ) := (ZZ) => (a,b) -> (
    b1 := b/1;
    return HilbertSymbolReal(a, b1);
    );

HilbertSymbolReal (ZZ, QQ) := (ZZ) => (a,b) -> (
    a1 := a/1;
    return HilbertSymbolReal(a1, b);
    );

HilbertSymbolReal (ZZ, ZZ) := (ZZ) => (a,b) -> (
    a1:= a/1;
    b1:= b/1;
    return HilbertSymbolReal(a1, b1);
    );


-- Input: Any integers a and b and a prime p. The integers a and b are considered as elements of QQ_p.
-- Output: The Hilbert symbol (a,b)_p following Serre III Theorem 1

HilbertSymbol = method()
HilbertSymbol (ZZ, ZZ, ZZ) := (ZZ) => (a, b, p) -> (
    if (a==0 or b==0) then error "first two arguments to HilbertSymbol must be nonzero";
    if (not isPrime(p)) then error "third argument of HilbertSymbol must be a prime";
    
    alpha := padicValuation(a,p);
    beta := padicValuation(b,p);
    u := sub(a/p^alpha,ZZ);
    v := sub(b/p^beta, ZZ);
    
    -- If epsilon(p) = 0
    if (p % 4 == 1) then(
	return ((squareSymbol(u,p))^beta * (squareSymbol(v,p))^alpha);
	);
    
    -- If epsilon(p) = 1
    if (p % 4 == 3) then(
	return ((-1)^(alpha*beta))*(((squareSymbol(u,p))^beta) * ((squareSymbol(v,p))^alpha));
	
	);
    
    -- Finally if p=2
    if p == 2 then(
	-- The reductions of u, v must be mod 8, as the calculation of (u-1)/2, (u^2-1)/8 below 
	-- depends on 
	-- the mod 8 reduction
	u = (u % 8);
	v = (v % 8);
	alpha = (alpha % 2);
	beta = (beta % 2);
	d := sub(((u-1)/2)*((v-1)/2) + alpha*((v^2-1)/8) + beta*((u^2-1)/8),ZZ);
	return ((-1)^d)
	);
    
    );

HilbertSymbol (QQ, QQ, ZZ) := (ZZ) => (a, b, p) -> (
 
-- if a, b are rational numbers with denominators,  one can multiply by square of denominator to 
-- get a', b' integers.  Then evaluate HilbertSymbol (a', b', p);
    
    if not liftable(a, ZZ) then (
	a1 := numerator a;
	a2 := denominator a;
	a = a1*a2;
	);
	
    if not liftable(b, ZZ) then (
	b1 := numerator b;
	b2 := denominator b;
	b = b1*b2;
	);
    
    a = sub(a,ZZ);
    b = sub(b,ZZ);
    return HilbertSymbol(a,b,p);
    );

HilbertSymbol (ZZ, QQ, ZZ) := (ZZ) => (a, b, p) -> (
    a1:=a/1;
    return HilbertSymbol(a1,b, p);    
   );

HilbertSymbol (QQ, ZZ, ZZ) := (ZZ) => (a, b, p) -> (
   b1:=b/1;
   return HilbertSymbol(a, b1, p);    
   );
