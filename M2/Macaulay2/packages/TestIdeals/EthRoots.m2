--*************************************************
--*************************************************
--This file is used for doing the [1/p^e] operation
--in the sense of Blickle-Mustata-Smith.
--This operation is also called I_e in Katzman or
--simply the image of
--M \subseteq \Hom_R(R^{1/p^e}, R) -> R
--under evaluation at 1.
--*************************************************
--*************************************************

frobeniusRoot = method( Options => { FrobeniusRootStrategy => Substitution } )
--frobeniusRoot takes two strategy options: Substitution and MonomialBasis
--The second strategy seems to generally be faster for computing I^[1/p^e] when e = 1, especially for polynomials of
--high degree, but slower for larger e.
-- Dan: I wonder if this is because getFieldGen is not optimized? It's called many times per
-- generator of the ideal in the monomial strategy. Though I see it's also called for the
-- substitution strategy...

frobeniusRoot ( ZZ, Ideal ) := Ideal => opts -> ( e, I ) ->
(
    if e < 0 then error "frobeniusRoot: Expected first argument to be a nonnegative integer";
    R := ring I;
    if class R =!= PolynomialRing then error "frobeniusRoot: Expected an ideal in a PolynomialRing";
    p := char R;
    if not isPrime p then error "frobeniusRoot: Expected an ideal in a ring of prime characteristic";
    k := coefficientRing R;
    if k =!= ZZ/p and class k =!= GaloisField then error "frobeniusRoot: Expected the coefficient field to be a finite prime field or a GaloisField";

    q := k#order;
    --Gets the cardinality of the base field.
    G := I_*;
    --Produces a list of the generators of I.
    if #G == 0 then ideal 0_R
    else
        if opts.FrobeniusRootStrategy == MonomialBasis then
        (
	    L := sum apply( G, f -> frobeniusRootMonStrat(e,p,q,k,f,R) );
    	    L = first entries mingens L;
	    ideal L
	)
        else frobeniusRootSubStrat(e,p,q,k,I,R)
)

-----------------------------------------------------------------------------

frobeniusRoot ( ZZ, MonomialIdeal ) := MonomialIdeal => opts -> ( e, I ) ->
(
    R := ring I;
    p := char R;
    G := I_*;
    if #G == 0 then ideal 0_R else monomialIdeal apply( G, f -> R_( (exponents f)#0 // p^e ))
)

------------------------------------------------------------------------------

frobeniusRoot ( ZZ, List, List ) := Ideal => opts -> ( e, exponentList, idealList ) ->
(
    --idealList is a list of ideals and/or ring elements.
    --exponentList is a list of exponents we're taking these ideals/elements to

    --include the following line to set a break point:
    --error "break here";
--    if (#idealList > 0) then (
--        if (instance(idealList#0, RingElement)) then (
--            return
--        );
--    );
    I := null;
    if e == 0 then
    (
        I = idealList#0^( exponentList#0 );
        apply( 1..(length(idealList) - 1), j-> I = I * ( idealList#j )^( exponentList#j ) );
        return I
    );

    R := ring idealList#0;
    p := char R;
    minGensList := apply( idealList, jj -> if class jj === Ideal then #(first entries mingens jj) else 1 );

    -- find max n such that a - (n-1)p > m*p. This is the number of copies of $I$ we can
    -- move outside the pth root.

    nsList := apply( exponentList, minGensList, ( aa, mm ) -> max( 0, floor( aa/p - mm + 1 ) ) );
    I = R;
    apply( length idealList, j -> I = I * ( idealList#j )^( exponentList#j - nsList#j * p ) );
    I = frobeniusRoot( 1, I, opts );
    frobeniusRoot( e - 1, append( nsList, 1 ), append( idealList, I ), opts )
)

-----------------------------------------------------------------------------

frobeniusRoot ( ZZ, ZZ, RingElement, Ideal ) := Ideal => opts -> ( e, a, f, I ) ->
    frobeniusRootRingElements ( e, a, f, I, opts )

-----------------------------------------------------------------------------

frobeniusRoot ( ZZ, ZZ, RingElement ) := Ideal => opts -> ( e, a, f ) ->
    frobeniusRootRingElements ( e, a, f, opts )

-----------------------------------------------------------------------------

frobeniusRoot ( ZZ, ZZ, Ideal ) := Ideal => opts -> ( e, m, I ) ->
    frobeniusRoot( e, {m}, {I}, opts )

-----------------------------------------------------------------------------

frobeniusRoot( ZZ, List, List, Ideal) := Ideal => opts -> (e, exponentList, idealList, J) ->
   frobeniusRoot(e, append(exponentList, 1), append(idealList, J), opts )

-----------------------------------------------------------------------------
frobeniusRoot ( ZZ, Module ) := Matrix => opts -> ( e, A ) ->
(
    if (isFreeModule super A) then (
        return image frobeniusRoot(e, generators A, opts);
    ) else (
        error "frobeniusRoot: Expected the second argument to be a submodule of a free module.";
    );
);

frobeniusRoot ( ZZ, Matrix ) := Matrix => opts -> ( e, A ) ->
(
    if e < 0 then error "frobeniusRoot: Expected first argument to be a nonnegative integer";
    R := ring A;
    if class R =!= PolynomialRing then error "frobeniusRoot: Expected a matrix with entries in a PolynomialRing";
    p := char R;
    if not isPrime p then error "frobeniusRoot: Expected a matrix with entries in a ring of prime characteristic";
    k := coefficientRing R;
    if k =!= ZZ/p and class k =!= GaloisField then error "frobeniusRoot: Expected the coefficient field to be a finite prime field or a GaloisField";
   mEthRoot( e, A )
)

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- MACHINERY
-----------------------------------------------------------------------------

getFieldGenRoot = (e, p, q, k) ->
(
    s := floorLog( p, q );
    -- Gets the exponent s such that q = p^s.
    a := (gens k)#0;
    a^(p^(s-e%s))
    -- Gets the p^e-th root of the cyclic generator a for the field extension k
    -- over ZZ/p.  If 1,a,..,a^t is a basis for k over ZZ/p and
    -- c = c_0 + c_1a + .. + c_ta^t in k, then replacing a with its p^e-th root
    -- in the preceding expansion using substitute(c,a => getFieldGenRoot(e,p,q,k))
    -- yields the p^e-th root of c.
)


-----------------------------------------------------------------------------

frobeniusRootMonStrat = (e, p, q, k, f, R) ->
(
    -- e = exponent, p = prime, q = size of coeff field, k = coeff field,
	-- f = a generator of the ideal in question, R = the ring
	-- to use this strategy to find the p^eth root of an ideal, you need to apply this
	-- function to each generator of the ideal and sum the results.
	-- maybe this should just return the ideal though? I guess it's an internal
	-- function, so it doesn't matter.
    expDecomp := apply(exponents(f),exponent->{coefficient(R_exponent,f)*R_(exponent //p^e),exponent%p^e});
    --Gets the exponent vectors of each monomial X^u of the polynomial f, and associates to u the two-element list whose
    --first entry is cX^v and second entry is w, where c is the coefficient of X^u in f and u = p^e*v + w.
    if q > p then (
	substRule := ( (gens k)#0 => getFieldGenRoot(e,p,q,k) );
	expDecomp = apply( expDecomp, pair -> { substitute( pair#0, substRule ), pair#1 } );
    );
    remainders := partition(x-> x#1, expDecomp);
    --Sorts the preceding list of two-element lists into a hash table with keys the remainder w of the exponent vector.
    --The value of each key is a list of two-element lists {cX^v,w} with the same remainder.
    remainders = applyValues(remainders,v->apply(v,w->(w#0)));
    --Forgets the second entry of each two-element list in the preceding hash table.
    remainders = applyValues(remainders,v->sum(v));
    --Adds together all the terms for each key w in the hash table to get the coefficient of the basis monomial X^w
    --for R over R^(p^e).
    return ideal(values(remainders))
)

-----------------------------------------------------------------------------

frobeniusRootSubStrat = (e, p, q, k, I, R) ->
(
    n := numgens R;
    Rvars := R_*;
    Y := local Y;
    S := k(monoid[(Rvars | toList(Y_1..Y_n)), MonomialOrder=>ProductOrder{n,n},MonomialSize=>64]);
    --Produces a polynomial ring with twice as many variables as R.  The peculiar notation in the previous two lines
    --is required to ensure that the variables of S are hidden from the user.  In particular, the variables in R_* are
    --still recognized as variables of R and not S, and the code will not break if the variables in R happen to be called
    --Y_i also.
    Svars := S_*;
    J := ideal(apply(n,i->Svars#(n+i) - Svars#i^(p^e)))*S;
    H := apply((substitute(I,S))_*, f -> f % J);
    --If we denote the variables in R as X_1 .. X_n, then this replaces each occurrence of X_i^(p^e) in the polynomial f
    --with a Y_i.
    L := sum(H, f -> ideal((coefficients(f,Variables => Rvars))#1));
    --Peals off the coefficients of the basis polynomials for R over R^(p^e) as polynomials in the Y_i, and produces the
    --ideal generated by these coefficient polynomials.
    L = first entries mingens L;
    subRelations := apply(n,i->Svars#(n+i) => Svars#i);
    if q > p then subRelations = subRelations|{(gens k)#0 => getFieldGenRoot(e,p,q,k)};
    L = apply(L, g ->substitute(g,subRelations));
    --Pushes the ideal of coefficient polynomials down to R by substituting Y_i => X_i.
    --q := k#order;
    --Gets the size of the base field.
    substitute(ideal L, R)
)

frobeniusRootRingElements = method(Options => {FrobeniusRootStrategy => Substitution});
--This tries to compute (f1^a1*f2^a2*...fk^ak*I)^{[1/p^e]} in such a way that we don't blow exponent buffers.  It can be much faster as well.
--We should probably just use it.  It relies on the fact that (f^(ap+b))^{[1/p^2]} = (f^a(f^b)^{[1/p]})^{[1/p]}.

--It's a special case of frobeniusRoot(ZZ, List, List) that's optimized for lots of principal ideals

frobeniusRootRingElements( ZZ, List, List, Ideal ) := o->( e, aList, elmtList, I ) -> (
    R := ring I;
    p := char R;

    aListRem := aList % p^e;
    aListQuot := aList // p^e;

    -- gives the basePexpansion of each element of aListRem
    -- expOfaList is thus a list of lists.
    expOfaList := apply(aListRem, z -> reverse toList baseP1( p, z, e ) );

    -- this computes { ... f_i^b_i ... } where b_i = a_i % p
    aPowerList := apply(elmtList, expOfaList, (f, z) -> f^(z#0));

    IN1 := I*ideal(product(aPowerList));
    if (e > 0) then (
        IN1 = frobeniusRoot( 1, IN1 );
        i := 1;
        while(i < e) do (
            aPowerList = apply(elmtList, expOfaList, (f, z) -> f^(z#i));
            IN1 = frobeniusRoot( 1, IN1*ideal(product(aPowerList)), o );
            i = i + 1;
        )
    );
    aPowerList = apply(elmtList, aListQuot, (f, z) -> f^z);
    IN1*ideal(product(aPowerList))
)

frobeniusRootRingElements( ZZ, Sequence, Sequence, Ideal ) := o->(a, b, c, d) -> frobeniusRootRingElements(a, toList b, toList c, d, o )

frobeniusRootRingElements( ZZ, ZZ, RingElement, Ideal ) := o->( e, a, f, I ) ->
    frobeniusRootRingElements(e, {a}, {f}, I, o )

frobeniusRootRingElements( ZZ, ZZ, RingElement ) := o->( e, a, f ) ->
    frobeniusRootRingElements( e, {a}, {f}, ideal( 1_(ring f) ), o )

----------------------------------------------------------------
--************************************************************--
--Functions for computing test ideals, and related objects.   --
--************************************************************--
----------------------------------------------------------------

--Finds the smallest phi-stable ideal containing the given ideal Jk
--in a polynomial ring Sk
--Jk is the given ideal, ek is the power of Frobenius to use, hk is the function to multiply
--trace by to give phi:  phi(_) = Tr^(ek)(hk._)
--This is based on ideas of Moty Katzman, and his star closure

--this is a new ascendIdeal written by Karl.  It ascends but does it in a possibly non-polynomial ring.
--the point is the ascending might be faster if we don't care about it mod a certain ideal.
ascendIdeal = method( Options => { FrobeniusRootStrategy => Substitution, AscentCount => false } )

ascendIdeal ( ZZ, RingElement, Ideal ) := o -> ( ek, hk, Jk ) ->
    ascendIdeal( ek, {1}, {hk}, Jk, o )

--Works like above ascendIdeal but tries to minimize the exponents elements are taken to
-- what's ak?  Karl: ak is the numerator of the exponent t = ak/(p^ek - 1)

ascendIdeal ( ZZ, ZZ, RingElement, Ideal ) := o -> ( ek, ak, hk, Jk ) ->
    ascendIdeal( ek, {ak}, {hk}, Jk, o )

--handles lists of hk to powers...
ascendIdeal ( ZZ, List, List, Ideal ) := o -> ( ek, akList,  hkList, Jk ) ->
(
    Rk := ring Jk;
    Ik := ideal Rk;
    Sk := ambient Rk;

    pp := char Sk;
    IN := sub(Jk, Sk);
    IP := ideal(0_Sk);
    i1 := 0;
     --we want to make the largest ideal that is phi-stable, following Moty Katzman's idea
     --we do the following
    while not isSubset(IN+Ik, IP+Ik) do
    (
        i1 = i1 + 1;
        --print "Step";
        IP = IN;
        IN = frobeniusRoot( ek, akList, hkList, IP, FrobeniusRootStrategy => o.FrobeniusRootStrategy) + IP
    );

    --trim the output
    if not o.AscentCount then trim (IP*Rk) else ( trim (IP*Rk), i1 )
)

-----------------------------------------------------------------------------
--- Extend the Frobenius p^e th roots and star operations to submodules of
--- free modules (over polynomial rings with *prime* coeeficient field)
--- This implements the methods described in
--- Moty Katzman and Wenliang Zhang's paper
--- "Annihilators of Artinian modules compatible with a Frobenius map"
--- Journal of Symbolic computation, 2014

-----------------------------------------------------------------------------

getCoeffsAndExps = method( TypicalValue => List )

--Input: a 1x1 matrix {{F}}
--Output: a list of pairs (coefficient, exponent list), one for each
-- term of F

getCoeffsAndExps Matrix := List => F ->
(
    f := first first entries F; -- get the single entry in F
    coeffs := flatten entries last coefficients f;
    exps := exponents f;
    apply( coeffs, exps, identity) -- build list of pairs
)

-*
getExponents = method()

getExponents Matrix := f ->
(
    answer := {};
    t := terms first first entries f;
    local c;
    local exps;
    apply( t, i ->
	(
            exps = first exponents i;
            c = (coefficients i)#1;
            c = first first entries c;
            answer = append( answer, ( c, exps ) )
        )
    );
    answer
)
*-

mEthRootOfOneElement = ( e, v ) ->
(
    local ww;
    local expVecModQ;
    local data;
    local key;
    local root;
    local rootOfGen;
    R := ring v;
    p := char R;
    q := p^e;
    var := R_*;
    n := rank target v;
    F := coefficientRing R;
    -- root = a function for computing p^e-th roots of the coefficients
    if isFinitePrimeField F then root = identity
    else
    (
	rootOfGen = getFieldGenRoot( e, p, F#order, F );
	root = c -> substitute( c, (gens F)#0 => rootOfGen )
    );
    T := new MutableHashTable;
    B := {};
    scan( n, i ->
	scan( getCoeffsAndExps v^{i}, ( coeff, expVec ) ->
	    (
		expVecModQ = expVec % q;
		B = append( B, expVecModQ );
		key = ( i, expVecModQ );
		-- create new monomials with the quotients of exponents by q
		data = root(coeff) * product apply( var, expVec//q, (x,y) -> x^y );
		if T#?key then T#key = T#key + data else T#key = data
	    )
	)
    );
    TT := apply( unique B, b ->
	(
            ww = apply( n, i -> if T#?(i,b) then T#(i,b) else 0_R );
	    transpose matrix { toList ww }
	)
    );
    fold( (i,j) -> i|j, TT )
)

-*
mEthRootOfOneElement= (e,v) ->(
	local i; local j;
	local d;
	local w;
	local m;
	local answer;
	R:=ring(v); p:=char R; q:=p^e;
	F:=coefficientRing(R);
	n:=rank source vars(R);
	V:=ideal vars(R);
	vv:=first entries vars(R);
	T:=new MutableHashTable;
	alpha:=rank target matrix(v);
	B:={};
	for i from 1 to alpha do
	{
		vi:=v^{i-1};
---print("i=",i);
---print("vi=",vi);
		C:=getCoeffsAndExps(vi);
---print(C);
		apply(C, c->
		{
			lambda:=c#0;
			beta:=c#1;
			gamma:=apply(beta, j-> (j%q));
			B=append(B,gamma);
			key:=(i,gamma);
---print(beta, #beta,vv);
			data:=apply(1..(#beta), j-> vv_(j-1)^((beta#(j-1))//q));
			data=lambda*product(toList data);
---print(beta, key, data);
			if (T#?key) then
			{
				T#key=(T#key)+data;
			}
			else
			{
				T#key=data;
			};
		});
	};
	B=unique(B);
	TT:=new MutableHashTable;
	apply(B, b->
	{
		ww:={};
		for i from 1 to alpha do if T#?(i,b) then ww=append(ww,T#(i,b)) else ww=append(ww,0_R);
		ww=transpose matrix {ww};
		TT#b=ww;
	});
	KEYS:=keys(TT);
	answer=TT#(KEYS#0);
	for i from 1 to (#KEYS)-1 do answer=answer | TT#(KEYS#i);
	answer
)

*-

mEthRoot = ( e, A ) ->
(
    answer := apply( rank source A, i -> mEthRootOfOneElement( e, A_{i} ) );
    --the above subscript denotes taking the ith column of A
    answer = if #answer == 0 then A else fold( (i,j) -> i|j, answer );
    mingens image answer
)

-*
mEthRoot = (e,A) ->(
	local i;
	local answer;
	                                 --i->first entries mEthRootOfOneElement (e, A_{i-1}));
	answer1:=apply(1..(rank source A), i-> mEthRootOfOneElement (e, A_{i-1}));
	--the above subscript denotes taking the ith column of A
	if (#answer1==0) then
	{
		answer=A;
	}
	else
	{
		answer=answer1#0;
		apply(2..(#answer1), i->answer=answer | answer1#(i-1));
		--this apply statement turns a list of columns into a matrix
		--is there no better way?

	    --answer = matrix toList answer1;
	};
	mingens( image answer )
)
*-

-- ascendModule is the implementation of the star closure operation described in M Katzman and
-- W. Zhang's "Annihilators of Artinian modules compatible with a Frobenius map"
-- Inputs:
--    a positive integer e
--    submodule A of a free module R^n OVER A PRIME FIELD.
--    n by n matrix U
-- Output:
--    the smallest submodule V of R^n containing A and which satisfies
--    U^(1+p+...+p^(e-1)) V\subset V^{[p^e]}
-- This is analogous to ascendIdeal, only for submodules of free modules.
ascendModule = method()

ascendModule(ZZ, Module, Matrix) := (e, A, U) ->
(
    if (isFreeModule super A) then (
        return image ascendModule(e, generators A, U);
    ) else (
        error "ascendModule: Expected the second argument to be a submodule of a free module.";
    );
);

ascendModule ( ZZ, Matrix, Matrix ) := ( e, A, U ) ->
(
    R := ring A;
    p := char R;
    if A == 0 then A
    else
    (
	flag := true;
	Ne := sum( e, i -> p^i );
	lastA := A;
	while flag do
	(
	    flag = false;
	    A1 := matrix entries mEthRoot( e, mingens image( U^Ne * lastA ) );
	    A1 = A1 | lastA;
	    t1 := compress ( A1 % lastA );
	    if t1 != 0 then
	    (
	        flag = true;
		lastA = mingens image A1;
	    );
	 );
	 mingens image A1
    )
--    use R;
--    answer
)
