newPackage(
        "Factor",
        Version => "0.1",
        Date => "Nov 29, 2020",
        Authors => {{Name => "Paul Zinn-Justin",
                  Email => "pzinn@unimelb.edu.au",
                  HomePage => "http://http://blogs.unimelb.edu.au/paul-zinn-justin/"}},
        Headline => "Proper factor",
	Keywords => {"Miscellaneous"},
        DebuggingMode => false,
	AuxiliaryFiles => false
        )

export {"FactorPolynomialRing"}

debug Core

commonPairs := (a,b,f) -> fusePairs(a,b, (x,y) -> if x === null or y === null then continue else f(x,y));
subPairs := (a,b) -> fusePairs(a,b, (x,y)-> if y===null then continue else if x===null then y else if y>x then y-x else continue);
-- mergePairs could be defined similarly as
-- mergePairs := (a,b,f) -> fusePairs(a,b, (x,y) -> if x === null then y else if y === null then x else f(x,y));

FactorPolynomialRing = new Type of PolynomialRing;
FactorPolynomialRing.synonym = "factorized polynomial ring";
coefficientRing FactorPolynomialRing := R -> coefficientRing last R.baseRings;
factor FactorPolynomialRing := opts -> identity;
expression FactorPolynomialRing := R -> if hasAttribute(R,ReverseDictionary) then expression getAttribute(R,ReverseDictionary) else (expression factor) (expression last R.baseRings)
describe FactorPolynomialRing := R -> Describe (expression factor) (describe last R.baseRings)
factor FractionField := opts -> F -> frac(factor last F.baseRings); -- simpler to do it in this order -- though needs more checking (see also below)

leadCoeff := x -> ( -- iterated leadCoefficient
    R := ring x;
    if class R === PolynomialRing then leadCoeff leadCoefficient x else
    if class R === QuotientRing or class R === GaloisField then leadCoeff lift(x,ambient R) else
    x);

factor PolynomialRing := opts -> R -> (
    local Rf;
    if R.?factor then (
	Rf=R.factor;
	)
    else (
	Rf=new FactorPolynomialRing of RingElement from R; -- not of R from R for subtle reasons: each such R gets its own addition law etc, cf enginering.m2
	R.factor=Rf;
	Rf.baseRings=append(R.baseRings,R);
	commonEngineRingInitializations Rf;
	if Rf.?frac then remove(Rf,global frac);   -- simpler to do it in this order -- though needs more checking (see also above)
        expression Rf := a -> (expression a#0)* product apply(a#1,(f,e)->(expression f)^e);
	factor Rf := opts -> identity;
	factor R := opts -> a -> new Rf from a; -- factor now uses the factorized ring
	value Rf := a->(a#0)*product(a#1,u->(u#0)^(u#1));
	raw Rf := a-> (raw a#0)*product(a#1,u->(raw u#0)^(u#1)); -- !!!
	if (options R).Inverses then (
	denominator Rf := a -> new Rf from { (denominator a#0)*product(a#1,(f,e)->(denominator f)^e), {} };
	numerator Rf := a -> new Rf from { numerator a#0, apply(a#1,(f,e)->(numerator f,e)) };
	)
	else
	(
	denominator Rf := a -> new Rf from { denominator a#0, {} };
	numerator Rf := a -> new Rf from { numerator a#0, a#1 };
	);
	new Rf from R := (A,a) -> (
	    if (options R).Inverses then (
		-- a bit of a hack if a==0, but works
		minexps:=min\transpose apply(toList (rawPairs(raw R.basering,raw a))#1,m->exponents(R.numallvars,m)); -- sadly, exponents doesn't take an optional Variables like coefficients... might wanna change that
		a=a*R_(-minexps); -- get rid of monomial in factor if a Laurent polynomial.
		c:=R_minexps;
		)
	    else c = 1_R;
	    fe := toList apply append(rawFactor raw a,(f,e)->(
		    ff:=new R from f;
		    if (options R).Inverses and ff!=0 then (c=c*(leadMonomial ff)^e; ff=ff*(leadMonomial ff)^(-1)); -- should only be used with Inverses=>true
		    if leadCoeff ff >= 0 then ff else (if odd e then c=-c; -ff),e)
		);
	    if liftable(fe#0#0,R.basering) then (
		-- factory returns the possible constant factor in front
		assert(fe#0#1 == 1);
		c = c*(fe#0#0);
		fe=drop(fe,1);
		);
	    { c, -- constant term
		sort fe }  -- technically the sort should be on f, not on fe -- but should be the same. warning, do not change/remove sorting, needed by mergePairs
	    );
	new Rf from RawRingElement := (A,a) -> new Rf from (new R from a); -- promote uses this
	-- various redefinitions (there might be a more clever way to automate this?)
	Rf.generators=apply(generators R,a->new Rf from a);
	Rf.indexSymbols=applyValues(R.indexSymbols,x->new Rf from x);
	Rf.indexStrings=applyValues(R.indexStrings,x->new Rf from x);
	Rf#0=new Rf from { 0_R, {} };
	Rf#1=new Rf from { 1_R, {} };
	-- then operations!
	Rf * Rf := (a,b) -> if a#0===0_R or b#0===0_R then 0_Rf else new Rf from { a#0*b#0, mergePairs(a#1,b#1,plus) }; -- ha!
	Rf ^ ZZ := (a,n) -> (
	if n>0 then new Rf from { a#0^n, apply(a#1,(f,e)->(f,e*n)) } else if n===0 then 1_Rf else if a#1 =!= {} then error "division is not defined in this ring" else new Rf from {(a#0)^n,{}} -- negative value of n can only occur for constant/monomial
	);
	- Rf := a -> new Rf from { -a#0, a#1 };
	-- to avoid #321
	--    gcd (Rf, Rf) := (a,b) -> new Rf from { gcd(a#0,b#0), if a#0==0 then b#1 else if b#0==0 then a#1 else commonPairs(a#1,b#1,min) }; -- commonPairs only adds keys in both
	--    lcm (Rf, Rf) := (a,b) -> new Rf from { lcm(a#0,b#0), mergePairs(a#1,b#1,max) }; -- ha!
	gcd (Rf, Rf) := (a,b) -> new Rf from { new R from rawGCD(raw a#0,raw b#0), if a#0===0_R then b#1 else if b#0===0_R then a#1 else commonPairs(a#1,b#1,min) }; -- commonPairs only adds keys in both
	lcm (Rf, Rf) := (a,b) -> a*(b//gcd(a,b)); -- yuck (there's no rawLCM)
	Rf // Rf := (a,b) -> (
	    if a#0===0_R then return 0_Rf;
	    mn:=subPairs(a#1,b#1);
	    mp:=subPairs(b#1,a#1);
	    if mn==={} and (a#0)%(b#0)===0_R then new Rf from { (a#0)//(b#0), mp } else new Rf from ((value new Rf from {a#0,mp})//(value new Rf from {b#0,mn}))
	);
	Rf + Rf := (a,b) ->  ( c:=gcd(a,b); c*(new Rf from (value(a//c)+value(b//c))) );
	Rf - Rf := (a,b) ->  ( c:=gcd(a,b); c*(new Rf from (value(a//c)-value(b//c))) );
	Rf == Rf := (a,b) -> ( c:=gcd(a,b); value(a//c) == value(b//c) ); -- this is almost, but not quite the same as asking for equality of every factor (!)
	-- ... and map (only really useful when target ring is also factorized, or map considerably reduces complexity of polynomial)
	RingMap Rf := (p,x) -> (
	R := source p;
	S := target p;
	local pp;
	if R === ring x then pp = a -> promote(rawRingMapEval(raw p,raw a),S) else pp = a -> promote(rawRingMapEval(raw p,raw promote(a,R)),S);
	-- should perhaps test if promote is possible, else error "ring element not in source of ring map, and not promotable to it";
	(pp(x#0))*product(x#1,u->(pp(u#0))^(u#1))
	);
	-- experimental
-*	lowestPart(ZZ,Rf) := (d,x) -> lowestPart x; -- no checking performed
	lowestPart Rf := x -> (new Rf from {x#0,{}}) * product(x#1,(f,e) -> (new Rf from lowestPart f)^e); *-
	remove(Rf,symbol vars); -- in case R had already cached its vars
	);
	Rf
    );

-- this is an optimization: the product would take forever
FactorPolynomialRing _ List := (R,v) -> (
    R0 := last R.baseRings;
    if (options R).Inverses then new R from { R0_v, {} }
    else new R from { 1_R, sort apply(#v, i-> (R0_i,v#i)) }
    );

-- force the use of the new factor
Ring Array := PolynomialRing => (R,variables) -> (
    RM := R monoid variables;
    factor RM := opts -> a -> (factor RM; factor a);
    use RM
    )
Ring List := PolynomialRing => (R,variables) -> (
    RM := R monoid (variables,Local => true);
    factor RM := opts -> a -> (factor RM; factor a);
    use RM
    )


FactorPolynomialRing#{Standard,AfterPrint}=Thing#{Standard,AfterPrint}
