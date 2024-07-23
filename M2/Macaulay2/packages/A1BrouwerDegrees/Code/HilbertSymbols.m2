-- Input: A pair of nonzero rational numbers a and b
-- Output: 1 if either a or b is positive; -1 if they are both negative
-- See Serre, III Theorem 1

getHilbertSymbolReal = method()
getHilbertSymbolReal (QQ, QQ) := ZZ => (a, b) -> (
    if (a == 0 or b == 0) then
	error "the arguments of getHilbertSymbolReal must be nonzero";
    if (a < 0 and b < 0) then (
	return -1;
	)
    else
	return 1;
    )

getHilbertSymbolReal (QQ, ZZ) := ZZ => (a, b) -> (
    b1 := b/1;
    getHilbertSymbolReal(a,b1)
    )

getHilbertSymbolReal (ZZ, QQ) := ZZ => (a, b) -> (
    a1 := a/1;
    getHilbertSymbolReal(a1,b)
    )

getHilbertSymbolReal (ZZ, ZZ) := ZZ => (a, b) -> (
    a1:= a/1;
    b1:= b/1;
    getHilbertSymbolReal(a1,b1)
    )

-- Input: A pair of rational numbers a and b and a prime number p. The integers a and b are considered as elements of QQ_p
-- Output: The Hilbert symbol (a,b)_p following Serre, III Theorem 1

getHilbertSymbol = method()
getHilbertSymbol (ZZ, ZZ, ZZ) := ZZ => (a, b, p) -> (
    if (a == 0 or b == 0) then
	error "first two arguments of getHilbertSymbol must be nonzero";
    if not isPrime p then
	error "third argument of getHilbertSymbol must be prime";
    
    alpha := getPadicValuation(a,p);
    beta := getPadicValuation(b,p);
    u := sub(a/p^alpha, ZZ);
    v := sub(b/p^beta, ZZ);
    
    if (p % 4 == 1) then
	return ((getSquareSymbol(u,p))^beta) * ((getSquareSymbol(v,p))^alpha);
    
    if (p % 4 == 3) then
	return ((-1)^(alpha*beta))*((getSquareSymbol(u,p))^beta) * ((getSquareSymbol(v,p))^alpha);
    
    if p == 2 then (
	-- The reductions of u and v are mod 8, as the calculation of (u-1)/2 and (u^2-1)/8 below 
	-- depends on their mod 8 reduction
	u = u % 8;
	v = v % 8;
	alpha = alpha % 2;
	beta = beta % 2;
	d := sub((u-1)*(v-1)/4 + alpha*(v^2-1)/8 + beta*(u^2-1)/8, ZZ);
	return (-1)^d;
	);
    )

getHilbertSymbol (QQ, QQ, ZZ) := ZZ => (a, b, p) -> (
 
-- If a and b are rational numbers with denominators, one can multiply by the square of their denominators to 
-- get integers a' and b' and then evaluate getHilbertSymbol (a',b',p)
    
    if not liftable(a, ZZ) then a = numerator(a)*denominator(a);
    if not liftable(b, ZZ) then b = numerator(b)*denominator(b);
    
    a = sub(a, ZZ);
    b = sub(b, ZZ);
    getHilbertSymbol(a,b,p)
    )

getHilbertSymbol (ZZ, QQ, ZZ) := ZZ => (a, b, p) -> (
    getHilbertSymbol(a/1,b, p)
    )

getHilbertSymbol (QQ, ZZ, ZZ) := ZZ => (a, b, p) -> (
   getHilbertSymbol(a,b/1, p)
   )
