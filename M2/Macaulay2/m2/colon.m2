-- Copyright 1996 by Michael E. Stillman

----------------
-- quotients ---
----------------

quot0 := (I,J,options) -> (
    -- this is the version when I, J are ideals,
    R := (ring I)/I;
    mR := transpose gens J ** R;
    g := syz gb(mR,
           options,
           Strategy=>LongPolynomial,
           Syzygies=>true,SyzygyRows=>1);
    -- The degrees of g are not correct, so we fix that here:
    -- g = map(R^1, null, g);
    lift(ideal g, ring I)
    )

quot1 := (I,J,options) -> (
    -- This is the iterative version, where I and J
    -- are ideals
    R := ring I;
    M1 := ideal(1_R);
    scan(numgens J, i -> (
       f := J_i;
       if gens(f*M1) % (gens I) != 0 then (
	    M2 := quotient(I,f);
	    M1 = intersect(M1,M2);)));
    M1)

quot2 := (I,J,options) -> (
     error "not implemented yet";
     -- linear case, I,J ideals homog. J=(x) linear
     )

quotmod0 := (M,J,options) -> (
     m := gens M;
     F := target m;
     mm := generators M;
     if M.?relations then mm = mm | M.relations;
     j := transpose gens J;
     g := (j ** F) | (target j ** mm);
     -- We would like to be able to inform the engine that
     -- it is not necessary to compute various of the pairs
     -- of the columns of the matrix g.
     h := syz gb(g, options,
	  Strategy=>LongPolynomial,
	  SyzygyRows=>numgens F,
	  Syzygies=>true);
     if M.?relations then
         subquotient(h % M.relations, 
	             M.relations)
     else
         image h
     )

quotmod1 := (I,J,options) -> (
    -- This is the iterative version, where I is a 
    -- submodule of F/K, or ideal, and J is an ideal.
    M1 := super I;
    m := gens I | relations I;
    scan(numgens J, i -> (
       f := J_i;
       if gens(f*M1) % m != 0 then (
	    M2 := quotient(I,f);
	    M1 = intersect(M1,M2);)));
    M1)

quotmod2 := (I,J,options) -> (
     error "not implemented yet";
     -- This is the case when J is a single linear 
     -- element, and everything is homogeneous
     )

quotann0 := (M,J,options) -> (
     m := gens M;
     if M.?relations then m = m | M.relations;
     j := adjoint(gens J, (ring J)^1, source gens J);
     F := target m;
     g := j | (source gens J ** m);
     -- << g << endl;
     -- We would like to be able to inform the engine that
     -- it is not necessary to compute various of the pairs
     -- of the columns of the matrix g.
     h := syz gb(g, options,
	  Strategy=>LongPolynomial,
	  SyzygyRows=>1,
	  Syzygies=>true);
     ideal h
     )

quotann1 := (I,J,options) -> (
    R := ring I;
    M1 := ideal(1_R);
    m := gens I | relations I;
    scan(numgens J, i -> (
       f := image (J_{i});
       if gens(f**M1) % m != 0 then (
	    M2 := quotient(I,f);
	    M1 = intersect(M1,M2);)));
    M1)


doQuotientOptions := (options) -> (
    options = new MutableHashTable from options;
    remove(options,Strategy);
    remove(options,MinimalGenerators);
    --options.SyzygyLimit = options.GeneratorLimit;
    --remove(options,GeneratorLimit);
    new OptionTable from options
    )

quotientIdeal := (I,J,options) -> (
     if ring I =!= ring J
       then error "expected ideals in the same ring";
     domins := options.MinimalGenerators;
     strat := options.Strategy;
     options = doQuotientOptions options;
     local IJ;
     if strat === quote Iterate then
         IJ = quot1(I,J,options)
     else if strat === quote Linear then
         IJ = quot2(I,J,options)
     else 
     	 IJ = quot0(I,J,options);
     if domins then trim IJ else IJ)

quotientModule := (I,J,options) -> (
     if ring I =!= ring J
       then error "expected same ring";
     domins := options.MinimalGenerators;
     strat := options.Strategy;
     options = doQuotientOptions options;
     local IJ;
     if strat === quote Iterate then
         IJ = quotmod1(I,J,options)
     else if strat === quote Linear then
         IJ = quotmod2(I,J,options)
     else 
     	 IJ = quotmod0(I,J,options);
     if domins then trim IJ else IJ)

quotientAnn := (I,J,options) -> (
     if ring I =!= ring J
       then error "expected same ring";
     domins := options.MinimalGenerators;
     strat := options.Strategy;
     options = doQuotientOptions options;
     local IJ;
     if strat === quote Iterate then
         IJ = quotann1(I,J,options)
     else if strat === quote Linear then
         error "'Linear' not allowable strategy"
     else
     	 IJ = quotann0(I,J,options);
     if domins then trim IJ else IJ)

quotient = method(
     Options => {
	  --DegreeLimit => {},
	  --GeneratorLimit => infinity,
	  --PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null
	  }
     )

quotient(Ideal,Ideal) := (I,J,options) ->
    quotientIdeal(I,J,options)

quotient(Ideal,RingElement) := (I,f,options) ->
    quotientIdeal(I,ideal(f),options)

quotient(Module,Ideal) := (M,I,options) ->
    quotientModule(M,I,options)

quotient(Module,RingElement) := (M,f,options) -> 
    quotientModule(M,ideal(f),options)

quotient(Module,Module) := (M,I,options) ->
    quotientAnn(M,I,options)

Ideal : Ideal :=
Ideal : RingElement :=
Module : Ideal :=
Module : RingElement :=
Module : Module := (I,J) -> quotient(I,J)

document { quote quotient,
    TT "quotient(I,J,options)", " -- computes the ideal or
        submodule quotient (I:J).", 
    BR,NOINDENT,
    TT "(I : J)", "  -- a nicer notation, but you cannot provide options",
    BR,NOINDENT,
    TT "quotient(Ideal,Ideal)",
    BR,NOINDENT,
    TT "quotient(Ideal,RingElement)",
    BR,NOINDENT,
    TT "quotient(Module,Ideal)",
    BR,NOINDENT,
    TT "quotient(Module,RingElement)",
    BR,NOINDENT,
    TT "quotient(Module,Module)",
    PARA,
    "If I and J are ideals, this is (I:J) = {x in R | xJ in I}.
    If I is a submodule of a (either free or quotient) module
    M, and J is an ideal, this is the set of m in M s.t.
    mJ in I.  Finally, if I and J are submodules of the same
    module M, then the result is the set of all a in the base
    ring R, s.t. aJ in I.",
    EXAMPLE "R = ZZ/32003[a..d]",
    EXAMPLE "I = monomialCurve(R,{1,4,7})",
    EXAMPLE "J = ideal(I_1-a^2*I_0,I_2-d*c*I_0)",
    EXAMPLE "J : I",
    PARA,
    "Allowable options include:",
    MENU {
        --TO "DegreeLimit",
	--TO "GeneratorLimit",
	--TO "PairLimit",
	TO "Strategy",
	TO "MinimalGenerators"
        },
    BR,
    "The strategy option value (if any) should be one of the following:",
    MENU {
	TO "Iterate",
        TO "Linear"
        },
    BR,
    "The computation is currently not stored anywhere: this means
    that the computation cannot be continued after an interrupt..
    This will be changed in a later version"
    }

TEST "
  -- quotient(Ideal,Ideal)
  -- quotient(Ideal,RingElement)
  -- options to test: DegreeLimit, GeneratorLimit, PairLimit, 
  --    MinimalGenerators,
  --    Strategy=>Iterate, Strategy=>Linear
  R = ZZ/101[a..d]
  I1 = monomialCurve(R, {1,3,7})
  I2 = ideal((gens I1)_{0,1})
  I3 = quotient(I2,I1)
  I4 = quotient(I2,I3)
  I5 = quotient(I2, c)

  assert(I2 == 
       intersect(I3,I4)
       )
  
  assert(ideal(c,d) ==
       quotient(I2, I5)
       )

  assert(I3 ==
       I2 : I1
       )

--  assert(ideal(d) + I2 ==
--       quotient(I2,I1,DegreeLimit=>1)
--       )

  assert(I3 ==
       quotient(I2,I1,Strategy=>Iterate)
       )
  
  quotient(I2,I1,MinimalGenerators=>false)
--  stderr << \"  -- this fails currently\" << endl
--  assert(I5 ==
--       quotient(I2, c,Strategy=>Linear)
--       )
  
"

TEST "
  -- quotient(Ideal,Ideal)
  -- quotient(Ideal,RingElement)
  -- options to test: DegreeLimit, GeneratorLimit, PairLimit, 
  --    MinimalGenerators,
  --    Strategy=>Iterate, Strategy=>Linear
  R = ZZ/101[vars(0..3)]/(a*d)
  I1 = ideal(a^3, b*d)
  I2 = ideal(I1_0)

  I3 = quotient(I2,I1)
  assert(I3 == ideal(a))
  I4 = quotient(I2,I3)
  assert(I4 == ideal(a^2,d))
  I5 = quotient(I2, d)
  assert(I5 == ideal(a))
"

TEST "
  --    quotient(Module,RingElement)
  --    quotient(Module,Ideal)
  
  -- This tests 'quotmod0' (default)
  R = ZZ/101[vars(0..4)]/e
  m = matrix{{a,c},{b,d}}
  M = subquotient(m_{0}, a^2**m_{0} | a*b**m_{1})
  J = ideal(a)
  Q1 = quotient(M,J)
  
  -- Now try the iterative version
  Q2 = quotient(M,J,Strategy=>Iterate)
  assert(Q1 == Q2)

  m = gens M  
  F = target m
  mm = generators M | relations M
  j = transpose gens J
  g = (j ** F) | (target j ** mm)
  h = syz gb(g, 
	  Strategy=>LongPolynomial,
	  SyzygyRows=>numgens F,
	  Syzygies=>true)
  trim subquotient(h % M.relations, 
             M.relations)
  
"

TEST "
  --    quotient(Module,Module)
  R = ZZ/101[a..d]
  M = image matrix{{a,b},{c,d}}
  N = super M
  I = quotient(M,N)
  assert(I ==
            quotient(M,N,Strategy=>Iterative)
	)
   
  assert(I == 
            M : N
	)
  assert(I ==
            ann(N/M)
	)
"

TEST "
  --    quotient(Module,Module)
  R = ZZ/101[vars(0..14)]
  N = coker genericMatrix(R,a,3,5)
  M = image N_{}
  I = quotient(M,N)
  assert(I ==
            quotient(M,N,Strategy=>Iterative)
	)
   
  assert(I == 
            M : N
	)
  assert(I ==
            ann(N/M)
	)
"

TEST "
  R = ZZ/101[a..d]
  M = coker matrix{{a,b},{c,d}}
  m1 = basis(2,M) ** R
  image m1
  M1 = subquotient(matrix m1, relations M)
  Q1 = M1 : a  
  Q2 = quotient(M1,ideal(a,b,c,d),Strategy=>Iterate)
  assert(Q1 == Q2)
"

TEST "
  R = ZZ/101[a..d]
  mrels = matrix{{a,b},{c,d}}
  mgens = matrix(R,{{1,0},{0,0}})
  M = trim subquotient(mgens, mrels)
  Q1 = quotient(image M_{},a*d-b*c)
  assert(Q1 == super M)  -- fails: bug in == ...
"
    
----------------
-- saturation --
----------------

saturate = method(
     Options => {
	  DegreeLimit => {},
	  --GeneratorLimit => infinity,
	  --PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null
	  }
     )

satideal0old := (I,J,options) -> (
    R := ring I;
    I = lift(I,ring presentation R);
    m := transpose gens J;
    while I != 0 do (
	S := (ring I)/I;
	m = m ** S;
	I = ideal syz gb(m, Syzygies => true);
        );
    -- lift(I,R)
    ideal (presentation ring I ** R)
    )
satideal0 := (I,J,options) -> (
    R := ring I;
    m := transpose gens J;
    while (
	S := (ring I)/I;
	m = m ** S;
	I = ideal syz gb(m, Syzygies => true);
	I != 0
        ) do ();
    -- lift(I,R)
    ideal (presentation ring I ** R)
    )

satideal1 := (I,f,options) -> (
    I1 := ideal(1_(ring I));
    f = f_0;
    while not(I1 == I) do (
	I1 = I;
	I = ideal syz gb(matrix{{f}}|gens I,
                SyzygyRows=>1,Syzygies=>true););
    I)

satideal2 := (I,f,options) -> (
    -- This version may be used if f is a linear form
    -- and I is a submodule
    -- We need an easy test whether the ring of I
    -- uses rev lex order
    R := ring I;
    f = f_0;
    -- either check that R is rev lex, or make new ring...
    if degree f === {1} then (
	res := newCoordinateSystem(R,matrix{{f}});
	fto := res#1;
	fback := res#0;
	J := fto gens I;
	gb(J,options);
	m := divideByVariable(gens gb J, R_(numgens R-1));
	ideal fback m)
    )

satideal3 := (I,f,options) -> (
     -- Bayer method.  This may be used if I,f are homogeneous.
     -- Basic idea: in a ring R[z]/(f-z), with the rev lex order,
     -- compute a GB of I.
     R := ring I;
     n := numgens R;
     f = f_0;
     degs := append((monoid R).degrees, degree f);
     R1 := (coefficientRing R)[Variables=>n+1,Degrees=>degs];
     i := map(R1,R,(vars R1)_{0..n-1});
     f1 := i f;
     I1 := ideal (i gens I);
     A := R1/(f1-R1_n);
     iback := map(R,A,vars R | f);
     IA := gens I1 ** A;
     g := gens gb(IA,options);
     g = divideByVariable(g, A_n);
     ideal iback g
     )

satideal4 := (I,f,options) -> (
     f = f_0;
     R := ring I;
     n := numgens R;
     R1 := (coefficientRing R)[Variables=>n+1,MonomialOrder=>Eliminate 1];
     fto := map(R1,R,genericMatrix(R1,R1_1,1,n));
     f1 := fto f;
     R2 := R1/(f1*R1_0-1);
     fback := map(R,R2,matrix{{0_R}} | vars R);
     fto =  map(R2,R,genericMatrix(R2,R2_1,1,n));
     II := ideal fto gens I;
     g := gb(II,options);
     p1 := selectInSubring(1, gens g);
     ideal fback p1)

removeOptions := (options, badopts) -> (
    options = new MutableHashTable from options;
    scan(badopts, k -> remove(options, k));
    new OptionTable from options)

saturate(Ideal,Ideal) := (J,I,options) -> (
    -- various cases here
    n := numgens I;
    R := ring I;
    if ring J =!= R then error "expected ideals in the same ring";
    linearvar := (n === 1 and degree(I_0) === {1});
    homog := isHomogeneous I and isHomogeneous J;
    
    strategy := options.Strategy;
    domins := options.MinimalGenerators;
    options = removeOptions(options, {MinimalGenerators,Strategy});
    if strategy === null then
       if linearvar and homog and isPolynomialRing R 
          then strategy = Linear
          else strategy = Iterate;

    local f;
    if strategy === Linear then
      (
	if not linearvar or not homog then error
	  "'Linear' method requires saturation w.r.t. single linear element";
        f = satideal2;
      )
    else if strategy === Bayer then 
      (
	if not homog
	then error "Bayer method cannot be used in inhomogeneous case";
	if n =!= 1
	then error "Bayer method only saturates w.r.t. a single element";
        f = satideal3;
      )
    else if strategy === Elimination then
      (
	if n =!= 1 then error
	  "'Elimination' method requires a single element";
        f = satideal4
      )
    else if strategy === Iterate then
        f = satideal0
    else
        f = satideal0;

    g := f(J,I,options);
    if domins then trim g else g
    )

saturate(Ideal, RingElement) := (I,f,options) -> saturate(I,ideal f,options)

saturate Ideal := (I,options) -> saturate(I,ideal vars ring I, options)

saturate(Module,Ideal) := (M,I,options) -> (
    -- various cases here
    M1 := M : I;
    while M1 != M do (
	 M = M1;
	 M1 = M : I;
	 );
    M)

saturate(Module,RingElement) := (M,f,options) -> saturate(M,ideal(f),options)

saturate(Module) := (M,options) -> saturate(M,ideal vars ring M, options)

saturate(Vector) := (v,options) -> saturate(submodule v, options)

document { quote MinimalGenerators,
     TT "MinimalGenerators => true", " -- an option for ", TO "saturate", "
     which specifies whether to compute minimal generators for the result.",
     PARA,
     "The default value is ", TT "true", "."
     }

document { quote Elimination,
     TT "Strategy => Elimination", " -- an option value for ", TO "saturate", " 
     which indicates that the saturation of (I:f) should be computed by
     eliminating z from (I,f*z-1), where z is a new variable."
     }

document { quote Bayer,
     TT "Strategy => Bayer", " -- an option value for ", TO "saturate", " which
     indicates that the method of Bayer's thesis should be used.",
     PARA,
     "The method is to compute saturate(I,f) for I and f homogeneous,
     add a new variable z, compute a groebner basis of (I,f-z) in reverse lex order,
     divide by z, and finally replace z by f."
     }

document { quote Iterate,
     TT "Strategy => Iterate", " -- an option value for ", TO "saturate", " which
     indicates that successive ideal or module quotients should be used.",
     PARA,
     "This value is the default."
     }

document { quote Linear,
     TT "Strategy => Linear", " -- an option value for ", TO "saturate", " which
     indicates that the reverse lex order should be used to compute the saturation.",
     PARA,
     "This presumes that J is a single, linear polynomial, and that I 
     is homogeneous.",
     PARA,
     "This is also an option value for ", TO "pushForward1", "."
     }
     

TEST "
-- The ideal case
R = ZZ/101[a..d]
I = monomialCurve(R,{1,3,4})
F = I_0
J = ideal(F*I_1, I_2, F^2*I_3)
saturate(J,F)

J = truncate(4,I)
time saturate(J,a,Strategy=>Bayer)
time saturate(J,a,Strategy=>Linear)
time saturate(J,a,Strategy=>Iterate)
time saturate(J,a,Strategy=>Default)
time saturate(J,a,Strategy=>Elimination)

time saturate(J,Strategy=>Default)
time saturate(J,Strategy=>Iterate)
assert(try saturate(J,Strategy=>Bayer) else true)
assert(try saturate(J,Strategy=>Linear) else true)
assert(try saturate(J,Strategy=>Elimination) else true)
"

TEST "
-- The module case
R = ZZ/101[a..d]
M = subquotient(matrix{{a^2,b^2},{a*d,c^2}}, matrix{{c},{d}})

time saturate(M,a)
time saturate(M,a,Strategy=>Bayer)
time saturate(M,a,Strategy=>Linear)
time saturate(M,a,Strategy=>Elimination)
time saturate(M,a,Strategy=>Iterate)
"

TEST "
R = ZZ/101[x,y,z,a,b,c,d]
S = ZZ/101[x,y,z]
row2 = substitute(random(S^1, S^{-3,-3,-3,-3}), R)
row1 = matrix{{a,b,c,d}}
J = minors(2,row1 || row2)
-- gbTrace 3
  -- best time so far for the following: 30.41 seconds
  -- but this doesn't yet include finding a minimal set
  -- of generators for the image
time saturate(J, ideal row2)
time saturate(J, ideal row2, Strategy=>Iterate)
time saturate(J, ideal row2, MinimalGenerators=>false)
  
  -- the time for the following is 40.58 seconds...
  -- but I think too many GB's are being done...
time (
  J1 = quotient(J, ideal row2);
  J2 = quotient(J1, ideal row2);
  J3 = quotient(J2, ideal row2);
  J4 = quotient(J3, ideal row2);
  )
"

TEST "
R = ZZ/101[x,y,z,a,b,c,d]
--R = ZZ/101[a,b,c,d,x,y,z]  This order is VERY BAD!!
--R = ZZ/101[x,y,z,a,b,c,d,MonomialOrder=>ProductOrder{3,4}]
S = ZZ/101[x,y,z]
row2 = substitute(random(S^1, S^{-3,-3,-3,-3}), R)
row1 = matrix{{a,b,c,d}}
J = minors(2,row1 || row2)
-- gbTrace 3
F = row2_(0,0)
  -- For this example, just saturate w.r.t. F.
  -- best time: 21.76 seconds
time saturate(J, F)
time saturate(J, F, Strategy=>Bayer)  -- 21.76
time saturate(J, F, Strategy=>Elimination) -- 26.08
"

TEST "
R = ZZ/101[a..f]
m = monomialCurve(R,{1,3,4})
I = ideal(d-c) + m 
saturate(I,a+b)
I
"

TEST ///
	R = ZZ/101[a..f]
	I = ideal (d^2, d*f, f^2)
	J = ideal (d,f)
	assert( saturate(I,J) == R )
///
