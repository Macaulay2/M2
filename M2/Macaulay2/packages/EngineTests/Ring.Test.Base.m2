makeElementGenerator = (R,gen) -> (
    firstelements := join({0_R, 1_R, gen}, for i from 1 to 200 list gen^i);
    nelements := 0;
    f := () -> (if nelements < #firstelements then (
            result := firstelements#nelements; 
            nelements = nelements+1;
            result) 
        else (
            result = random(R);
            nelements = nelements+1;
            result));
    R#"nextElementGenerator" = f;
    R#"resetGen" = () -> nelements = 0;
    R#"indexGen" = () -> nelements;
    )
nextElement = method()
nextElement Ring := (R) -> R#"nextElementGenerator"()

resetGenerator = method()
resetGenerator Ring := (R) -> R#"resetGen"();

testFFAxioms = (R, ntrials) -> (
    for i from 1 to ntrials do (
        a := nextElement R;
        b := nextElement R;
        c := nextElement R;

        -- Test commutativity
        -- test: a*b = b*a
        -- test: a+b == b+a
        d := a+b;
        e := b+a;
        assert(d == e);
        d = a*b;
        e = b*a;
        assert(d == e);
    
        -- Test associativity
        -- test: a+(b+c) == (a+b)+c
        -- test: a*(b*c) == (a*b)*c
        d = a+(b+c);
        e = (a+b)+c;
        assert(d == e);
        d = a*(b*c);
        e = (a*b)*c;
        assert(d == e);

        -- Test distributivity
        -- test: a*(b+c) == a*b + a*c
        assert(a*(b+c) == a*b + a*c);
        assert((a+b)*c == a*c + b*c);
        
        -- identity elements
        assert(a+0 == a);
        assert(0*a == 0);
        assert(1*a == a);
        );
    )

testFFNegate = (R, ntrials) -> (
    resetGenerator R;
    for i from 1 to ntrials do (
        a := nextElement R;
        assert((-a)+(a) == 0_R);
        );
    )

testFFAdd = (R, ntrials) -> (
    resetGenerator R;
    for i from 1 to ntrials do (
        a := nextElement R;
        b := nextElement R;
        assert((a+b)+(-b) == a);
        );
    )

testFFSubtract = (R, ntrials) -> (
    resetGenerator R;
    for i from 1 to ntrials do (
        a := nextElement R;
        b := nextElement R;
        assert((a+b)-b == a);
        );
    )

testFFMultiply = (R, ntrials) -> (
    resetGenerator R;
    for i from 1 to ntrials do (
        a := nextElement R;
        b := nextElement R;
        assert((a+b)*(a+b) - a*a - b*b == 2*a*b);
        );
    )

testFFDivide = (R, ntrials) -> (
    resetGenerator R;        
    for i from 1 to ntrials do (
        a := nextElement R;
        b := nextElement R;
        if b != 0 then assert((a/b) * b == a);
        if b != 0 and a != 0 then assert(a/(a/b) == b);
        );
    )

testFFReciprocal = (R, ntrials) -> (
    resetGenerator R;        
    for i from 1 to ntrials do (
        a := nextElement R;
        if a != 0 then assert((1/a) * a == 1_R);
        );
    )

testFFPower = (R, ntrials) -> (
    resetGenerator R;
    q := R.order;
    assert((0_R)^2 == 0_R);
    assert((1_R)^0 == 1_R);
    for i from 1 to ntrials do (
        a := nextElement R;
        b := nextElement R;
        if a == 0 then continue;
        assert(a^q == a);
        assert(a^(q-1) == 1_R);
        assert(a^(-1) == 1/a);
        assert(a*a*a == a^3);
        assert(a*a == a^2);
        assert(a^1 == a);
        assert(a^0 == 1_R);
        );
    )

TEST ///
  restart
  debug loadPackage "EngineTests"
  R = GF(2^5)
  time testFF(R,R_0,1000)

  R = GF(2^5, Strategy=>"FlintBig")
  time testFF(R,R_0,1000)

  R = GF(2^5, Strategy=>"Flint")
  time testFF(R,R_0,1000)

///

testFF = (R, gen, ntrials) -> (
    makeElementGenerator(R, gen);
    -- should we assume that R_0 is the generator?  Probably not...
    testFFAxioms(R, ntrials);
    testFFNegate(R, ntrials);
    testFFAdd(R, ntrials);
    testFFSubtract(R, ntrials);
    testFFMultiply(R, ntrials);
    testFFDivide(R, ntrials);
    testFFReciprocal(R, ntrials);
    testFFPower(R, ntrials);
--    testCoercions(R,gen);
    )

-- also test: R as a coeff ring to a poly ring
--            promote, lift, eval, random, syzygy (? why? who cares...)
